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

guessit(Directory, Settings, Options) ->
    guessit(Directory, Settings, Options, ?GuessitRetries).

guessit(Directory, #{guessit := #{"ip" := IP, "port" := Port}}=Settings, Options, RetriesLeft) ->
    DirectoryName = filename:basename(Directory),
    case gun:open(IP, Port) of
        {ok, ConnPid} ->
	    StreamRef = gun:get(ConnPid, "/?filename=" ++ http_uri:encode(DirectoryName)),
	    case gun:await_body(ConnPid, StreamRef, ?GuessitTimeout) of
		{ok, Body} ->
		    gun:close(ConnPid),
                    GuessitData = jsx:decode(Body, [return_maps]),
                    UpdatedGuessitData = change_type(GuessitData, Options),
                    add(UpdatedGuessitData, ?GuessitName,
                        list_to_bitstring(DirectoryName));

		{error, Reason} when RetriesLeft > 0 ->
		    gun:close(ConnPid),
		    lager:warning("Request to guessit failed: ~p~n", [Reason]),
		    lager:info("Will retry ~p more time(s)~n", [RetriesLeft]),
		    guessit(Directory, Settings, Options, RetriesLeft - 1);

		{error, Reason} ->
		    gun:close(ConnPid),
		    lager:error("Request to guessit failed: ~p~n", [Reason]),
		    lager:error("Guessit retry threshold reached, giving up :(~n"),
                    unpacker_misc:halt(?EGuessitTimeout)
            end;
        {error, Reason} ->
            lager:info("Failed to open connection to guessit:~p~n", [Reason]),
            unpacker_misc:halt(?EGuessitConnect)
    end;

guessit(_, Settings, _Options, _RetriesLeft) ->
    lager:error("No guessit settings found in Settings data:~p~n", [Settings]),
    unpacker_misc:halt(?ENoGuessitSettings).


change_type(GuessitData, #{type := movie}) ->
    update(GuessitData, ?GuessitType, ?GuessitMovie);
change_type(GuessitData, #{type := tv}) ->
    update(GuessitData, ?GuessitType, ?GuessitEpisode);
change_type(GuessitData, _Options) ->
    GuessitData.

update(GuessitData, Key, Value) ->
    case maps:update(Key, Value, GuessitData) of
        {badKey, Key} ->
            lager:error("Couldn't update value ~p in key ~p for "
                        "guessit info:~p~n", [Value, Key, GuessitData]),
            unpacker_misc:halt(?EUpdateGuessitType);
        UpdatedGuessitData ->
            UpdatedGuessitData
    end.

add(GuessitData, Key, Value) ->
    maps:put(Key, Value, GuessitData).
