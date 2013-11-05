-module(raven_tests).

-include_lib("eunit/include/eunit.hrl").
-include("raven.hrl").

parser_test_() ->
    {setup,
        spawn,
        fun setup/0,
        fun teardown/1,
        fun blueprints/1}.

scan(Format, Dir) ->
    lists:reverse(filelib:fold_files(Dir, ".*[.]md", true, fun(File, Files) -> [{Format, File} | Files] end, [])).

setup() ->
    scan(blueprint, "../deps/api-blueprint/examples").

teardown(_Files) ->
    ok.

blueprints(Files) ->
    {inorder, [{timeout, 60, ?_test(blueprint(File))} || File <- Files]}.

blueprint({Format, File}) ->
    {ok, Data} = file:read_file(File),
    {ok, Result, Blueprint} = raven:parse(Format, Data),
    ?assertMatch([],Result#result.warnings),
    ?assertEqual(ok,Result#result.error#source_annotation.type),
    ?debugFmt("~p : ~p", [File, Result#result.error#source_annotation.type]),
    ok.
