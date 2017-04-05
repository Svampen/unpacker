%%%-------------------------------------------------------------------
%%% @author Stefan Hagdahl
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Mar 2017 16:34
%%%-------------------------------------------------------------------
-module(unpacker_misc).
-author("Stefan Hagdahl").

%% API
-export([halt/1]).

halt(Status) ->
    %% Fixes issue that lager can't write all logs before erlang terminates
    timer:sleep(2000),
    erlang:halt(Status).
