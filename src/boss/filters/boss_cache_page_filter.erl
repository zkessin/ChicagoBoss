-module(boss_cache_page_filter).
-export([before_filter/3, middle_filter/2, after_filter/2]).

-define(PAGE_CACHE_PREFIX, "boss_web_controller_page").
-define(PAGE_CACHE_DEFAULT_TTL, 60).

before_filter(RequestContext, ControllerModule, {RequestMethod, Action, Tokens}) ->
    CacheInfo = boss_cache_vars_filter:cache_info(RequestContext, ControllerModule, {RequestMethod, Action, Tokens}),

    Language = proplists:get_value(language, RequestContext, auto),

    CacheKey = {ControllerModule, Action, Tokens, Language},

    {DoCache, CacheOptions, CachedRenderedResult} = case CacheInfo of
        {page, Options} -> 
            {true, Options, boss_cache:get(?PAGE_CACHE_PREFIX, CacheKey)};
        _ -> {false, [], undefined}
    end,

    case CachedRenderedResult of
        undefined ->
            {ok, RequestContext ++ [{cache_page, DoCache}, {cache_options, CacheOptions}, {cache_key, CacheKey}]};
        _ ->
            {cached_page, CachedRenderedResult}
    end.

middle_filter({cached_page, CachedRenderedResult}, _RequestContext) ->
    CachedRenderedResult;
middle_filter(Other, _RequestContext) ->
    Other.

after_filter(Rendered, RequestContext) ->
    ReallyDoCache = (proplists:get_value(cache_page, RequestContext, false) andalso 
                     is_tuple(Rendered) andalso element(1, Rendered) =:= ok),
    case ReallyDoCache of
        true ->
            CacheKey = proplists:get_value(cache_key, RequestContext),
            CacheOptions = proplists:get_value(cache_options, RequestContext, []),
            CacheTTL = proplists:get_value(seconds, CacheOptions, ?PAGE_CACHE_DEFAULT_TTL),
            case proplists:get_value(watch, CacheOptions) of
                undefined -> ok;
                CacheWatchString ->
                    boss_news:set_watch({?PAGE_CACHE_PREFIX, CacheKey}, CacheWatchString,
                        fun boss_web_controller:handle_news_for_cache/3, {?PAGE_CACHE_PREFIX, CacheKey}, CacheTTL)
            end,
            boss_cache:set(?PAGE_CACHE_PREFIX, CacheKey, Rendered, CacheTTL);
        false ->
            ok
    end,
    Rendered.
