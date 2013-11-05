-type str() :: 'undefined' | nonempty_string().
-type key_value_pair() :: {str(), str()}.
-type key_value_pairs() :: [key_value_pair()].
-type warning_type() :: 'ok' | 'api_name' | 'duplicate' | 'formatting' | 'redefinition' | 'ignoring' | 'empty_definition' | 'not_empty_definition' | 'logical_error' | 'deprecated'.
-type error_type() :: 'ok' | 'application' | 'business' | 'symbol'.
-type use() :: 'undefined' | 'optional' | 'required'.

-record(source_annotation, {
    code :: 0..9,
    type :: warning_type() | error_type(),
    message :: str(),
    locations :: [{non_neg_integer(), non_neg_integer()}]
}).

-record(result, {
    error :: #source_annotation{},
    warnings :: [#source_annotation{}]
}).

-record(parameter, {
    name :: str(),
    description :: str(),
    type :: str(),
    use :: use(),
    default_value :: str(),
    example_value :: str(),
    values :: [str()]
}).

-record(payload, {
    name :: str(),
    description :: str(),
    parameters :: [#parameter{}],
    headers :: key_value_pairs(),
    body :: str(),
    schema :: str()
}).

-record(transaction_example, {
    name :: str(),
    description :: str(),
    requests :: [#payload{}],
    responses :: [#payload{}]
}).

-record(action, {
    method :: str(),
    name :: str(),
    description :: str(),
    parameters :: [#parameter{}],
    headers :: key_value_pairs(),
    examples :: [#transaction_example{}]
}).

-record(resource, {
    uri_template :: str(),
    name :: str(),
    description :: str(),
    model :: #payload{},
    parameters :: [#parameter{}],
    headers :: key_value_pairs(),
    actions :: [#action{}]
}).

-record(resource_group, {
    name :: str(),
    description :: str(),
    resources :: [#resource{}]
}).

-record(blueprint, {
    metadata :: key_value_pairs(),
    name :: str(),
    description :: str(),
    resource_groups :: [#resource_group{}]
}).
