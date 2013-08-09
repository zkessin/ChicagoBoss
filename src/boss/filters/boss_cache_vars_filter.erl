-module(boss_cache_vars_filter).
-export([before_filter/3, middle_filter/2]).

-define(VARIABLES_CACHE_PREFIX, "boss_web_controller_variables").
-define(VARIABLES_CACHE_DEFAULT_TTL, 60).

cache_info([{request, Req}|_] = RequestContext, Module, {EffectiveRequestMethod, Action, Tokens}) ->
    EffectiveRequestMethod = case Req:request_method() of
        'HEAD' -> 'GET';
        Method -> Method
    end,

    case (boss_env:get_env(cache_enable, false) andalso
            EffectiveRequestMethod =:= 'GET') of
        true ->
            case proplists:get_value('cache_', Module:module_info(exports)) of
                2 -> Module:cache_(Action, Tokens);
                3 -> Module:cache_(Action, Tokens, RequestContext);
                _ -> none
            end;
        false -> none
    end.

before_filter(RequestContext, ControllerModule, {RequestMethod, Action, Tokens}) ->
    CacheInfo = cache_info(RequestContext, ControllerModule, {RequestMethod, Action, Tokens}),

    Language = proplists:get_value(language, RequestContext, auto),

    CacheKey = {ControllerModule, Action, Tokens, Language},

    DoCache = case CacheInfo of
        {vars, _} -> true;
        _ -> false
    end,

    CachedActionResult = case CacheInfo of
        {vars, _} -> boss_cache:get(?VARIABLES_CACHE_PREFIX, CacheKey);
        _ -> undefined
    end,

    CacheOptions = case CacheInfo of
        {vars, Options} -> Options;
        _ -> []
    end,

    case CachedActionResult of
        undefined -> 
            {ok, RequestContext ++ [{cache_vars, DoCache}, {cache_options, CacheOptions}, {cache_key, CacheKey}]};
        _ -> CachedActionResult
    end.

middle_filter(ActionResult, RequestContext) ->
    ReallyDoCache = (proplists:get_value(cache_vars, RequestContext, false) andalso 
                     is_tuple(ActionResult) andalso element(1, ActionResult) =:= render),
    case ReallyDoCache of
        true ->
            CacheKey = proplists:get_value(cache_key, RequestContext),
            CacheOptions = proplists:get_value(cache_options, RequestContext, []),
            CacheTTL = proplists:get_value(seconds, CacheOptions, ?VARIABLES_CACHE_DEFAULT_TTL),
            case proplists:get_value(watch, CacheOptions) of
                undefined -> ok;
                CacheWatchString ->
                    boss_news:set_watch({?VARIABLES_CACHE_PREFIX, CacheKey}, CacheWatchString,
                        fun ?MODULE:handle_news_for_cache/3, {?VARIABLES_CACHE_PREFIX, CacheKey},
                        CacheTTL)
            end,
            boss_cache:set(?VARIABLES_CACHE_PREFIX, CacheKey, ActionResult, CacheTTL);
        false ->
            ok
    end,
    ActionResult.

