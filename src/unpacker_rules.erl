%%%-------------------------------------------------------------------
%%% @author Stefan Hagdahl
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Mar 2017 13:55
%%%-------------------------------------------------------------------
-module(unpacker_rules).
-author("Stefan Hagdahl").

-include("unpacker.hrl").

%% API
-export([rules/1,
         match/2]).

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

match(_, []) ->
    {error, "No rule matching Guessit information"};
match(#{?GuessitType := ?GuessitMovie}=Guessit,
    [#{rule := Rule, rule_opts :=
    #{"type" := ?RuleMovie,
      "extraction_location" := ExtractionLocation}=RuleOptions}|Rest]) ->
    lager:info("Processing Rule:~p~nRule Options:~p~n", [Rule, RuleOptions]),
    case match_rule_options(maps:to_list(RuleOptions), Guessit) of
        true ->
            {match, Rule, ExtractionLocation};
        false ->
            match(Guessit, Rest)
    end;
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
match_rule_options([{"screen_size", RuleScreenSize}|Rest],
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
match_rule_options([{"regex", Regex}|Rest], #{?GuessitTitle := Title}=Guessit) ->
    TitleString = bitstring_to_list(Title),
    case re:run(TitleString, Regex) of
        {match, _} ->
            match_rule_options(Rest, Guessit);
        nomatch ->
            false
    end;
match_rule_options([_RuleOption|Rest], Guessit) ->
    match_rule_options(Rest, Guessit).

split_screen_size("4K") ->
    split_screen_size("2160p");

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
