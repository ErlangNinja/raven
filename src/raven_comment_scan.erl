-module(raven_comment_scan).

-include_lib("eunit/include/eunit.hrl").
-include("raven.hrl").

-export([file/1, file/2]).

file(File) ->
    file(blueprint, File).

file(Format, File) ->
    Comments = erl_comment_scan:file(File),
    Blueprint = merge_blueprints(Comments),
    raven:parse(Format, Blueprint).

merge_blueprints([]) ->
    [];

merge_blueprints([{_Line, 1, _Indentation, Text} | Comments]) ->
    StrippedText = [strip_prefix(Line) || Line <- Text],
    extract_blueprint(StrippedText, [], false) ++ merge_blueprints(Comments);

merge_blueprints([_Comment | Comments]) ->
    merge_blueprints(Comments).

strip_prefix("% " ++ Line) ->
    strip_prefix(Line);

strip_prefix("%" ++ Line) ->
    strip_prefix(Line);

strip_prefix(Line) ->
    Line.

extract_blueprint([], Blueprint, _) ->
    Blueprint;

extract_blueprint(["@@blueprint" ++ _Line | Lines], Blueprint, _) ->
    extract_blueprint(Lines, Blueprint, true);

extract_blueprint(["@@end" ++ _Line | Lines], Blueprint, _) ->
    extract_blueprint(Lines, Blueprint, false);

extract_blueprint([Line | Lines], Blueprint, true) ->
    extract_blueprint(Lines, Blueprint ++ Line ++ "\n", true);

extract_blueprint([_Line | Lines], Blueprint, false) ->
    extract_blueprint(Lines, Blueprint, false).
