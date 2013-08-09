-module(boss_lang_filter).
-export([before_filter/3, after_filter/2]).

before_filter(ReqContext, ControllerModule, Location) ->
    LangResult = case proplists:get_value('lang_', ControllerModule:module_info(exports)) of
        1 -> ControllerModule:lang_(Location);
        2 -> ControllerModule:lang_(Location, ReqContext);
        _ -> auto
    end,
    case LangResult of
        auto -> {ok, proplists:delete(language, ReqContext)};
        Language -> {ok, proplists:delete(language, ReqContext) ++ [{language, Language}]}
    end.

after_filter({ok, Payload, Headers}, ReqContext) ->
    case proplists:get_value(language, ReqContext) of
        undefined -> {ok, Payload, Headers};
        Language -> {ok, Payload, boss_web_controller:merge_headers(Headers, [{"Content-Language", Language}])}
    end.
