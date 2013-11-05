-record(source_annotation, {
    code,
    type,
    message,
    locations
}).

-record(result, {
    error,
    warnings
}).

-record(parameter, {
    name,
    description,
    type,
    use,
    default_value,
    example_value,
    values
}).

-record(transaction_example, {
    name,
    description,
    requests,
    responses
}).

-record(action, {
    method,
    name,
    description,
    parameters,
    headers,
    examples
}).

-record(payload, {
    name,
    description,
    parameters,
    headers,
    body,
    schema
}).

-record(resource, {
    uri_template,
    name,
    description,
    model,
    parameters,
    headers,
    actions
}).

-record(resource_group, {
    name,
    description,
    resources
}).

-record(blueprint, {
    metadata,
    name,
    description,
    resource_groups
}).
