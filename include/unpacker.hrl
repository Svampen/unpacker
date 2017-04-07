%%%-------------------------------------------------------------------
%%% @author Stefan Hagdahl
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Feb 2017 17:13
%%%-------------------------------------------------------------------
-author("Stefan Hagdahl").

-define(Config, "config.yaml").
-define(RegexRar,
    "^((?!sample)(?!part).)*\.rar|^((?!sample).)*(?:part[0]+1)\.rar").
-define(RegexVideo, "^((?!sample)(?!part).)*\.(mkv|mp4)").

-define(GuessitType, <<"type">>).
-define(GuessitTv, <<"episode">>).
-define(GuessitMovie, <<"movie">>).
-define(GuessitScreenSize, <<"screen_size">>).
-define(GuessitFormat, <<"format">>).
-define(GuessitSeason, <<"season">>).
-define(GuessitEpisode, <<"episode">>).
-define(GuessitTitle, <<"title">>).
-define(GuessitName, <<"name">>).
-define(GuessitYear, <<"year">>).

-define(RuleTv, "tv").
-define(RuleMovie, "movie").


%% Halt reasons
-define(ENoDir, 1).
-define(ENoCfgData, 2).
-define(EParseRules, 3).
-define(EYamlDocs, 4).
-define(EVerifyRules, 5).
-define(EGuessitConnect, 6).
-define(EGuessitTimeout, 7).
-define(ERuleMatch, 8).
-define(ENoConfigFile, 9).
-define(ENoGuessitSettings, 10).
-define(ENoSettingsData, 11).
-define(EUpdateGuessitType, 12).

-define(GuessitTimeout, 10000).
-define(GuessitRetries, 5).

-define(UnknownSeason, "S00").
