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
