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
