%%%-------------------------------------------------------------------
%%% @author Stefan Hagdahl
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Feb 2017 17:07
%%%-------------------------------------------------------------------
-module(unpacker).
-author("Stefan Hagdahl").

-include("unpacker.hrl").
-include_lib("mockgyver/include/mockgyver.hrl").

%% API
-export([unpacker/2]).

%%--------------------------------------------------------------------
%% @doc
%% Unpack/copy content from 'Directory' to location from matching rule
%% 1. Lookup if directory contains rar files or just video files
%% 2. If rar files then look inside to find which files to actually
%%    unpack
%% 3. Request information about directory using guessit, requires that
%%    guessit is installed as REST
%% 4. Parse rules with guessit information as input to determine
%%    where directory content should be unpacked/copy to
%% 5. Unpack/copy directory content to destination
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec(unpacker(Directory :: string(), Options :: map()) ->
    ok | {error, Reason :: term()}).
unpacker(Directory, Options) ->
    application:ensure_all_started(gun),
    application:start(yamerl),
    lager:start(),
    lager:set_loglevel(lager_backend_console, debug),
    case Options of
        #{test := "yes"} ->
            stub(Directory, Options);
        _ ->
            start(Directory, Options)
    end.

start(Directory, Options) ->
    ok = validate_dir(Directory),
    {Rules, Settings} = unpacker_config:get(Options),
    Files = probe_directory(Directory, Settings),
    Guessit = guessit(Directory, Settings),
    lager:info("Guessit:~p~n", [Guessit]),
    case unpacker_rules:match(Guessit, Rules) of
        {match, Rule, ExtractionLocation} ->
            lager:info("Found matching rule:~p~n", [Rule]),
            Destination = create_final_destination(Guessit, ExtractionLocation),
            unpack(Directory, Files, Destination);
        {error, Reason} ->
            lager:error("Match error:~p~n", [Reason]),
            halt(?ERuleMatch)
    end.

validate_dir(Directory) ->
    case filelib:is_dir(Directory) of
        true ->
            ok;
        false ->
            lager:error("Directory doesn't exists:~p~n", [Directory]),
            unpacker_misc:halt(?ENoDir)
    end.

probe_directory(Directory, Settings) ->
    RegexVideo =
    case Settings of
        #{regex := Regex} ->
            Regex;
        _ ->
            ?RegexVideo
    end,

    RarFun =
    fun(RarFile, Acc) ->
        VideoFiles = unrar:list(RarFile),
        Acc ++ [#{rar_file => RarFile, video_files => VideoFiles}]
    end,
    RarFiles = filelib:fold_files(Directory, ?RegexRar, true, RarFun, []),
    VideoFun =
    fun(VideoFile, Acc) ->
        Acc ++ [VideoFile]
    end,
    VideoFiles = filelib:fold_files(Directory, RegexVideo, true, VideoFun, []),
    #{rar_files => RarFiles, video_files => VideoFiles}.

guessit(Directory, #{guessit := #{"ip" := IP, "port" := Port}}) ->
    DirectoryName = filename:basename(Directory),
    case gun:open(IP, Port) of
        {ok, ConnPid} ->
            {ok, _} = gun:await_up(ConnPid),
            StreamRef = gun:get(ConnPid, "/?filename=" ++ DirectoryName),
            receive
                {gun_data, ConnPid, StreamRef, fin, Response} ->
                    jiffy:decode(Response, [return_maps])
            after
                2000 ->
                    lager:error("Connection to guessit timed out~n"),
                    unpacker_misc:halt(?EGuessitTimeout)
            end;
        {error, Reason} ->
            lager:info("Failed to open connection to guessit:~p~n", [Reason]),
            unpacker_misc:halt(?EGuessitConnect)
    end;
guessit(_, Settings) ->
    lager:error("No guessit settings found in Settings data:~p~n", [Settings]),
    unpacker_misc:halt(?ENoGuessitSettings).

create_final_destination(#{?GuessitType := ?GuessitTv,
                           ?GuessitSeason := GuessitSeason,
                           ?GuessitTitle := GuessitTitle},
                         ExtractionLocation) ->
    Title = string:join(string:tokens(bitstring_to_list(GuessitTitle), " "), "."),
    Season =
    if
        GuessitSeason < 10 ->
            lists:concat(["S0", GuessitSeason]);
        true ->
            lists:concat(["S", GuessitSeason])
    end,
    filename:join([ExtractionLocation, Title, Season]);
create_final_destination(#{?GuessitType := ?GuessitMovie,
                           ?GuessitTitle := GuessitTitle},
                         ExtractionLocation) ->
    Title = string:join(string:tokens(bitstring_to_list(GuessitTitle), " "), "."),
    filename:join([ExtractionLocation, Title]).

unpack(_Directory, #{rar_files := RarFiles, video_files := VideoFiles},
       Destination) ->
    lager:info("Ensuring that Destination exists:~p~n", [Destination]),
    case filelib:is_dir(Destination) of
        true ->
            ok;
        false ->
            ok = filelib:ensure_dir(Destination),
            ok = file:make_dir(Destination)
    end,
    lists:foreach(
        fun(#{rar_file := RarFile,
                video_files := RarVideoFiles}) ->
            lager:info("Extracting from Rar file:~p~nVideo files:~p~n",
                       [RarFile, RarVideoFiles]),
            unrar:extract(RarFile, RarVideoFiles, Destination);
           (_) ->
               ok
        end,
        RarFiles),
    lists:foreach(
        fun(VideoFile) ->
            lager:info("Copying VideoFile(~p) to destination:~p~n",
                       [VideoFile, Destination]),
            unpacker_misc:copy(VideoFile, Destination)
        end,
        VideoFiles),
    ok.

%% stub functions
stub(Directory, Options) ->
    ?MOCK(fun() ->
        ?WHEN(unrar:extract(RarFile, RarVideoFiles, Destination) ->
              lager:info("Extracting from ~p to ~p the following files:~p~n",
                         [RarFile, RarVideoFiles, Destination])),
        ?WHEN(unpacker_misc:copy(VideoFile, Destination) ->
              lager:info("Coping ~p to ~p~n", [VideoFile, Destination])),
        ?WHEN(unpacker:halt(Status) ->
              begin
              lager:info("Halting erlang with status:~p in 2 seconds~n",
                         [Status]),
              timer:sleep(2000)
              end),
        start(Directory, Options)
        end).