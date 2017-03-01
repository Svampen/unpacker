%%%-------------------------------------------------------------------
%%% @author Stefan Hagdahl
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Mar 2017 13:49
%%%-------------------------------------------------------------------
-module(unpacker_config).
-author("Stefan Hagdahl").

-include("unpacker.hrl").

%% API
-export([get/1]).

get(Options) ->
    Config = get_config(Options),
    Rules = unpacker_rules:rules(Config),
    Settings = settings(Config),
    {Rules, Settings}.

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
build_settings([[{"regex", Regex}]|Rest], Settings) ->
    UpdatedSettings = maps:put(regex, Regex, Settings),
    build_settings(Rest, UpdatedSettings);
build_settings([Setting|Rest], Settings) ->
    lager:warning("Unsupported setting found:~p~n", [Setting]),
    build_settings(Rest, Settings).

