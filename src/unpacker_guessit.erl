%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 17. Mar 2017 13:56
%%%-------------------------------------------------------------------
-module(unpacker_guessit).

%% API
-export([guessit/3]).

-include("unpacker.hrl").

guessit(Directory, #{guessit := #{"ip" := IP, "port" := Port}}, Options) ->
    DirectoryName = filename:basename(Directory),
    case gun:open(IP, Port) of
        {ok, ConnPid} ->
            {ok, _} = gun:await_up(ConnPid),
            StreamRef = gun:get(ConnPid, "/?filename=" ++ DirectoryName),
            receive
                {gun_data, ConnPid, StreamRef, fin, Response} ->
                    GuessitData = jsx:decode(Response, [return_maps]),
                    update_guessit(GuessitData, Options)
            after
                ?GuessitTimeout ->
                    lager:error("Connection to guessit timed out~n"),
                    unpacker_misc:halt(?EGuessitTimeout)
            end;
        {error, Reason} ->
            lager:info("Failed to open connection to guessit:~p~n", [Reason]),
            unpacker_misc:halt(?EGuessitConnect)
    end;
guessit(_, Settings, _Options) ->
    lager:error("No guessit settings found in Settings data:~p~n", [Settings]),
    unpacker_misc:halt(?ENoGuessitSettings).

update_guessit(GuessitData, #{type := movie}) ->
    case maps:update(?GuessitType, ?GuessitMovie, GuessitData) of
        {badKey, Key} ->
            lager:error("Couldn't update type ~p in key ~p for "
                        "guessit info:~p~n", [movie, Key, GuessitData]),
            unpacker_misc:halt(?EUpdateGuessitType);
        UpdatedGuessitData ->
            UpdatedGuessitData
    end;
update_guessit(GuessitData, #{type := tv}) ->
    case maps:update(?GuessitType, ?GuessitEpisode, GuessitData) of
        {badKey, Key} ->
            lager:error("Couldn't update type ~p in key ~p for "
                        "guessit info:~p~n", [tv, Key, GuessitData]),
            unpacker_misc:halt(?EUpdateGuessitType);
        UpdatedGuessitData ->
            UpdatedGuessitData
    end;
update_guessit(GuessitData, _Options) ->
    GuessitData.