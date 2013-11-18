-module(raven_tests).

-include_lib("eunit/include/eunit.hrl").
-include("raven.hrl").

-export([raven_parse/1, raven_comment_scan/1]).

parser_test_() ->
    {setup,
        spawn,
        fun setup/0,
        fun teardown/1,
        fun blueprints/1}.

scan(Format, Dir, Pattern, Fun) ->
    lists:reverse(filelib:fold_files(Dir, Pattern, true, fun(File, Files) -> [{Format, File, Fun} | Files] end, [])).

setup() ->
    scan(blueprint, "../blueprints", ".*[.]md", raven_parse) ++
    scan(blueprint, "../blueprints", ".*[.]erl", raven_comment_scan).

teardown(_Files) ->
    ok.

blueprints(Files) ->
    {Blueprints, _} = lists:foldl(fun({Format, File, Fun}, {Tests, Count}) ->
        Test = {File, Count, fun() -> ?MODULE:Fun({Format, File}) end},
        {[Test | Tests], Count + 1}
    end, {[], 1}, Files),
    {"Blueprints", inorder, {timeout, 60, lists:reverse(Blueprints)}}.

raven_parse({Format, File}) ->
    {ok, Data} = file:read_file(File),
    {ok, Result, _Blueprint} = raven:parse(Format, Data),
    ?assertMatch([], Result#result.warnings),
    ?assertEqual(ok, Result#result.error#source_annotation.type),
    ?debugFmt("~s : ~p", [File, Result#result.error#source_annotation.type]),
    ok.

raven_comment_scan({Format, File}) ->
    {ok, Result, _Blueprint} = raven_comment_scan:file(Format, File),
    ?assertMatch([], Result#result.warnings),
    ?assertEqual(ok, Result#result.error#source_annotation.type),
    ?debugFmt("~s : ~p", [File, Result#result.error#source_annotation.type]),
    ok.
