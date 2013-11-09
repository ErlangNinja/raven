-module(raven).

-include("raven.hrl").

-export([parse/1, parse/2]).

-on_load(load_nif/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).

load_nif() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    SoName = filename:join(PrivDir, ?MODULE),
    erlang:load_nif(SoName, {1, 0, 0}).

-type parse_result() :: {'error', #result{}} | {'ok', #result{}, str() | #blueprint{}}.

-spec parse(Data :: iolist()) -> parse_result().
parse(Data) ->
    parse(blueprint, Data).

-spec parse(Format :: 'blueprint' | 'json' | 'yaml', Data :: iolist()) -> parse_result().
parse(_, _) ->
    ?nif_stub.
