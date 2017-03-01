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
    ok = validate(Directory),
    Config = get_config(Options),
    Rules = rules(Config),
    Settings = settings(Config),
    Files = probe_directory(Directory),
    Guessit = guessit(Directory, Settings),
    lager:info("Guessit:~p~n", [Guessit]),
    case match(Guessit, Rules) of
        {match, Rule, ExtractionLocation} ->
            lager:info("Found matching rule:~p~n", [Rule]),
            Destination = create_final_destination(Guessit, ExtractionLocation),
            unpack(Directory, Files, Destination);
        {error, Reason} ->
            lager:error("Match error:~p~n", [Reason]),
            halt(?ERuleMatch)
    end.

validate(Directory) ->
    case filelib:is_dir(Directory) of
        true ->
            ok;
        false ->
            lager:error("Directory doesn't exists:~p~n", [Directory]),
            erlang:halt(?ENoDir)
    end.

get_config(#{config_file := ConfigFile}) ->
    %% check for config file in Options
    case filelib:is_file(ConfigFile) of
        true ->
            read_config(ConfigFile);
        false ->
            lager:error("User supplied config file not found:~p~n", [ConfigFile]),
            erlang:halt()
    end;
get_config(_Options) ->
    %% Use default path for config file
    {ok, DefaultPath} = file:get_cwd(),
    ConfigFile = filename:join([DefaultPath, ?Config]),
    case filelib:is_file(ConfigFile) of
        true ->
            read_config(ConfigFile);
        false ->
            lager:error("Default config file not found:~p~n", [ConfigFile]),
            erlang:halt()
    end.

read_config(ConfigFile) ->
    case yamerl_constr:file(ConfigFile) of
        [] ->
            lager:error("No data found in config file:~p~n", [ConfigFile]),
            erlang:halt(?ENoCfgData);
        [Config] ->
            lager:debug("Found config data:~p~n", [Config]),
            Config;
        _Config ->
            lager:error("To many yaml documents found in config file:~p~n",
                        [ConfigFile]),
            erlang:halt(?EYamlDocs)
    end.

settings(Config) ->
    case find_settings(Config) of
        {ok, Settings} ->
            lager:info("Settings:~p~n", [Settings]),
            build_settings(Settings, #{});
        error ->
            lager:error("No settings data found in config:~p~n", [Config]),
            erlang:halt(?ENoSettingsData)
    end.

find_settings([]) ->
    error;
find_settings([{"settings", Settings}|_Rest]) ->
    {ok, Settings};
find_settings([_|Rest]) ->
    find_settings(Rest).

build_settings([], Settings) ->
    Settings;
build_settings([[{"guessit", GuessitOptions}]|Rest], Settings) ->
    GuessitOptionsMap = maps:from_list(GuessitOptions),
    UpdatedSettings = maps:put(guessit, GuessitOptionsMap, Settings),
    build_settings(Rest, UpdatedSettings);
build_settings([Setting|Rest], Settings) ->
    lager:warning("Unsupported setting found:~p~n", [Setting]),
    build_settings(Rest, Settings).

rules(Config) ->
    case find_rules(Config) of
        {ok, Rules} ->
            lager:info("Rules:~p~n", [Rules]),
            case verify_rules(Rules) of
                {error, Reason} ->
                    lager:error("Failed to verify Rules:~p~nConfig"
                                ":~p~nReason:~p~n",
                                [Rules, Config, Reason]),
                    erlang:halt(?EVerifyRules);
                MapRules ->
                    MapRules
            end;
        {error, Reason} ->
            lager:error("Failed to parse rules in config"
                        ":~p~nError:~p~n", [Config, Reason]),
            erlang:halt(?EParseRules)
    end.
find_rules([]) ->
    {error, "No Rules found"};
find_rules([{"rules", Rules}|_Rest]) ->
    {ok, Rules};
find_rules([_|Rest]) ->
    find_rules(Rest).

verify_rules([]) ->
    [];
verify_rules([[{Rule, RuleOptions}]|Rest]) ->
    RuleOptionsMap = maps:from_list(RuleOptions),
    case verify_rule_options(RuleOptionsMap) of
        false ->
            {error, "Missing mandatory fields in rule: " ++ Rule};
        true ->
            [#{rule => Rule, rule_opts => RuleOptionsMap}] ++
            verify_rules(Rest)
    end.

verify_rule_options(#{"type" := _, "extraction_location" := _}) ->
    true;
verify_rule_options(_RuleOptions) ->
    false.

probe_directory(Directory) ->
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
    VideoFiles = filelib:fold_files(Directory, ?RegexVideo, true, VideoFun, []),
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
                    erlang:halt(?EGuessitTimeout)
            end;
        {error, Reason} ->
            lager:info("Failed to open connection to guessit:~p~n", [Reason]),
            erlang:halt(?EGuessitConnect)
    end;
guessit(_, Settings) ->
    lager:error("No guessit settings found in Settings data:~p~n", [Settings]),
    erlang:halt(?ENoGuessitSettings).

match(_, []) ->
    {error, "No rule matching Guessit information"};
match(#{?GuessitType := ?GuessitTv}=Guessit,
      [#{rule := Rule, rule_opts :=
      #{"type" := ?RuleTv,
        "extraction_location" := ExtractionLocation}=RuleOptions}|Rest]) ->
    lager:info("Processing Rule:~p~nRule Options:~p~n", [Rule, RuleOptions]),
    case match_rule_options(maps:to_list(RuleOptions), Guessit) of
        true ->
            {match, Rule, ExtractionLocation};
        false ->
            match(Guessit, Rest)
    end;
match(Guessit, [_Rule|Rest]) ->
    match(Guessit, Rest).

match_rule_options([], _Guessit) ->
    true;
match_rule_options([{"screen_size", RuleScreenSize}| Rest],
                   #{?GuessitScreenSize := GuessitScreenSize}=Guessit) ->
    {Range, RuleScreenSizeInt} = split_screen_size(RuleScreenSize),
    {_, GuessitScreenSizeInt} = split_screen_size(bitstring_to_list(GuessitScreenSize)),
    lager:info("Range:~p GuessitScreenSize:~p RuleScreenSize:~p",
               [Range, GuessitScreenSizeInt, RuleScreenSizeInt]),
    case Range of
        "-" ->
            if
                GuessitScreenSizeInt =< RuleScreenSizeInt ->
                    match_rule_options(Rest, Guessit);
                true ->
                    false
            end;
        "+" ->
            if
                GuessitScreenSizeInt >= RuleScreenSizeInt ->
                    match_rule_options(Rest, Guessit);
                true ->
                    false
            end;
        no_range ->
            if
                GuessitScreenSizeInt == RuleScreenSizeInt ->
                    match_rule_options(Rest, Guessit);
                true ->
                    false
            end
    end;
match_rule_options([_RuleOption|Rest], Guessit) ->
    match_rule_options(Rest, Guessit).

split_screen_size(ScreenSize) ->
    case string:substr(ScreenSize, string:len(ScreenSize), 1) of
        "-" ->
            {"-", list_to_integer(string:substr(ScreenSize, 1,
                                                string:len(ScreenSize)-2))};
        "+" ->
            {"+", list_to_integer(string:substr(ScreenSize, 1,
                                                string:len(ScreenSize)-2))};
        "p" ->
            {no_range, list_to_integer(string:substr(ScreenSize, 1,
                                                     string:len(ScreenSize)-1))}
    end.

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
                       [RarFile, RarVideoFiles]);
            %%unrar:extract(RarFile, RarVideoFiles, Destination);
           (_) ->
               ok
        end,
        RarFiles),
    lists:foreach(
        fun(VideoFile) ->
            lager:info("Copying VideoFile(~p) to destination:~p~n",
                       [VideoFile, Destination]),
            _CopyCommand = lists:concat(["cp ", VideoFile, " ", Destination])
            %%os:cmd(CopyCommand)
        end,
        VideoFiles),
    ok.
