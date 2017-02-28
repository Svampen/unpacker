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

-define(RuleTv, "tv").
-define(RuleMovie, "movie").


%% Halt reasons
-define(ENODIR, 1).
-define(ENOCFGDATA, 2).
-define(EPARSERULES, 3).
-define(EYAMLDOCS, 4).
-define(EVERIFYRULES, 5).
-define(EGUESSITCONNECT, 6).
-define(EGUESSITTIMEOUT, 7).
-define(ERULEMATCH, 8).