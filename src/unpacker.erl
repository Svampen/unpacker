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

%% API
-export([main/1]).

main([Directory|Rest]) ->
    Options = build_options(Rest, #{}),
    io:format("options:~p~n", [Options]),
    unpacker(Directory, Options).

build_options([], Options) ->
    Options;
build_options(["-test", Test|Rest], Options) ->
    build_options(Rest, maps:put(test, list_to_atom(Test), Options));
build_options(["-config", ConfigFile|Rest], Options) ->
    build_options(Rest, maps:put(config_file, ConfigFile, Options));
build_options(["-type", Type|Rest], Options) ->
    build_options(Rest, maps:put(type, list_to_atom(Type), Options));
build_options(Options, _) ->
    io:format("Unsupported options found:~p~n", [Options]),
    erlang:halt(1).

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
    ok = validate_dir(Directory),
    {Rules, Settings} = unpacker_config:get(Options),
    Files = probe_directory(Directory, Settings),
    Guessit = unpacker_guessit:guessit(Directory, Settings, Options),
    lager:info("Guessit:~p~n", [Guessit]),
    case unpacker_rules:match(Guessit, Rules) of
        {match, Rule, ExtractionLocation} ->
            lager:info("Found matching rule:~p~n", [Rule]),
            Destination = create_final_destination(Guessit, ExtractionLocation),
            unpack(Directory, Files, Destination, Options);
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

create_final_destination(#{?GuessitType := ?GuessitTv,
                           ?GuessitTitle := GuessitTitle}=Guessit,
                         ExtractionLocation) ->
    GuessitTitleString = bitstring_to_list(GuessitTitle),
    Title = string:join(string:tokens(GuessitTitleString, " "), "."),
    Season =
    case maps:get(?GuessitSeason, Guessit, undefined) of
        undefined ->
            lager:warning("No season info found for Guessit:~p~n", [Guessit]),
            ?UnknownSeason;
        GuessitSeason when is_integer(GuessitSeason) ->
            if GuessitSeason < 10 ->
                lists:concat(["S0", GuessitSeason]);
                true ->
                    lists:concat(["S", GuessitSeason])
            end;
        GuessitSeason ->
            lager:error("Unknown season value ~p for Guessit:~p~n",
                        [GuessitSeason, Guessit]),
            ?UnknownSeason
    end,
    filename:join([ExtractionLocation, Title, Season]);
create_final_destination(#{?GuessitType := ?GuessitMovie,
                           ?GuessitTitle := GuessitTitle},
                         ExtractionLocation) ->
    GuessitTitleString = bitstring_to_list(GuessitTitle),
    Title = string:join(string:tokens(GuessitTitleString, " "), "."),
    filename:join([ExtractionLocation, Title]).

unpack(_Directory, #{rar_files := RarFiles, video_files := VideoFiles},
       Destination, #{test := yes}) ->
    lager:info("Would have ensured that ~p exists", [Destination]),
    lists:foreach(
        fun(#{rar_file := RarFile,
                video_files := RarVideoFiles}) ->
            lager:info("Would have extracted from Rar file:~p~n"
                       "Video files:~p~n",
                       [RarFile, RarVideoFiles]);
           (_) ->
               ok
        end,
        RarFiles),
    lists:foreach(
        fun(VideoFile) ->
            lager:info("Would have copied VideoFile(~p) to destination:~p~n",
                       [VideoFile, Destination])
        end,
        VideoFiles),
    ok;
unpack(_Directory, #{rar_files := RarFiles, video_files := VideoFiles},
       Destination, _Options) ->
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
