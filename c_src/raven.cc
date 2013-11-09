#include <iostream>
#include <sstream>
#include <fstream>
#include "erl_nif.h"
#include "snowcrash.h"
#include "SerializeJSON.h"
#include "SerializeYAML.h"

using namespace snowcrash;

const char *error_str[] = {
    "ok",
    "application",
    "business",
    "symbol"
};

const char *warning_str[] = {
    "ok",
    "api_name",
    "duplicate",
    "formatting",
    "redefinition",
    "ignoring",
    "empty_definition",
    "not_empty_definition",
    "logical_error",
    "deprecated"
};

const char *use_str[] = {
    "undefined",
    "optional",
    "required"
};

// static variables
static ERL_NIF_TERM a_json;
static ERL_NIF_TERM a_yaml;
static ERL_NIF_TERM a_ok;
static ERL_NIF_TERM a_error;
static ERL_NIF_TERM a_undefined;
static ERL_NIF_TERM a_not_implemented;

static ERL_NIF_TERM a_result;
static ERL_NIF_TERM a_blueprint;
static ERL_NIF_TERM a_source_annotation;
static ERL_NIF_TERM a_resource_group;
static ERL_NIF_TERM a_resource;
static ERL_NIF_TERM a_payload;
static ERL_NIF_TERM a_parameter;
static ERL_NIF_TERM a_action;
static ERL_NIF_TERM a_transaction_example;

static ERL_NIF_TERM l_empty;

static void init_terms(ErlNifEnv* env) {

    a_json = enif_make_atom(env, "json");
    a_yaml = enif_make_atom(env, "yaml");

    a_ok = enif_make_atom(env, "ok");
    a_error = enif_make_atom(env, "error");
    a_undefined = enif_make_atom(env, "undefined");
    a_not_implemented = enif_make_atom(env, "not_implemented");

    a_result = enif_make_atom(env, "result");
    a_blueprint = enif_make_atom(env, "blueprint");
    a_source_annotation = enif_make_atom(env, "source_annotation");
    a_resource_group = enif_make_atom(env, "resource_group");
    a_resource = enif_make_atom(env, "resource");
    a_payload = enif_make_atom(env, "payload");
    a_parameter = enif_make_atom(env, "parameter");
    a_action = enif_make_atom(env, "action");
    a_transaction_example = enif_make_atom(env, "transaction_example");

    l_empty = enif_make_list(env, 0);

}

static int on_load(ErlNifEnv* env, void**, ERL_NIF_TERM) {
    init_terms(env);
    return 0;
}

static ERL_NIF_TERM wrap_string(ErlNifEnv* env, const std::string str) {
    if (str.empty()) {
        return a_undefined;
    } else {
        return enif_make_string(env, str.c_str(), ERL_NIF_LATIN1);
    }
}

static ERL_NIF_TERM wrap_annotation(ErlNifEnv* env, const SourceAnnotation annotation, const char *dict[]) {
    ERL_NIF_TERM code = enif_make_int(env, annotation.code);
    ERL_NIF_TERM type = enif_make_atom(env, dict[annotation.code]);
    ERL_NIF_TERM message = wrap_string(env, annotation.message);
    ERL_NIF_TERM list = enif_make_list(env, 0);
    if (!annotation.location.empty()) {
        for (SourceCharactersBlock::const_iterator it = annotation.location.begin();it != annotation.location.end();++it) {
            ERL_NIF_TERM loc = enif_make_int(env, it->location);
            ERL_NIF_TERM len = enif_make_int(env, it->length);
            ERL_NIF_TERM tuple = enif_make_tuple2(env, loc, len);
            list = enif_make_list_cell(env, tuple, list);
        }
    }
    return enif_make_tuple5(env, a_source_annotation, code, type, message, list);
}

static ERL_NIF_TERM wrap_warnings(ErlNifEnv* env, const Warnings warnings) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Warnings::const_iterator it = warnings.begin(); it != warnings.end(); ++it) {
        list = enif_make_list_cell(env,wrap_annotation(env,*it,warning_str),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_result(ErlNifEnv* env, const Result result) {
    return enif_make_tuple3(env, a_result, wrap_annotation(env,result.error,error_str), wrap_warnings(env,result.warnings));
}

static ERL_NIF_TERM wrap_kvpairs(ErlNifEnv* env, const Collection<KeyValuePair>::type kvpairs) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Collection<KeyValuePair>::const_iterator it = kvpairs.begin(); it != kvpairs.end(); ++it) {
        ERL_NIF_TERM key = wrap_string(env, it->first);
        ERL_NIF_TERM value = wrap_string(env, it->second);
        list = enif_make_list_cell(env,enif_make_tuple2(env,key,value),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_values(ErlNifEnv* env, const Collection<Value>::type values) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Collection<Value>::const_iterator it = values.begin(); it != values.end(); ++it) {
        list = enif_make_list_cell(env,wrap_string(env,*it),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_parameter(ErlNifEnv* env, const Parameter parameter) {
    ERL_NIF_TERM name = wrap_string(env, parameter.name);
    ERL_NIF_TERM description = wrap_string(env, parameter.description);
    ERL_NIF_TERM type = wrap_string(env, parameter.type);
    ERL_NIF_TERM use = enif_make_atom(env, use_str[parameter.use]);
    ERL_NIF_TERM default_value = wrap_string(env, parameter.defaultValue);
    ERL_NIF_TERM example_value = wrap_string(env, parameter.exampleValue);
    ERL_NIF_TERM values = wrap_values(env, parameter.values);
    return enif_make_tuple8(env, a_parameter, name, description, type, use, default_value, example_value, values);
}

static ERL_NIF_TERM wrap_parameters(ErlNifEnv* env, const Collection<Parameter>::type parameters) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Collection<Parameter>::const_iterator it = parameters.begin(); it != parameters.end(); ++it) {
        list = enif_make_list_cell(env,wrap_parameter(env,*it),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_payload(ErlNifEnv* env, const Payload payload) {
    ERL_NIF_TERM name = wrap_string(env, payload.name);
    ERL_NIF_TERM description = wrap_string(env, payload.description);
    ERL_NIF_TERM parameters = wrap_parameters(env, payload.parameters);
    ERL_NIF_TERM headers = wrap_kvpairs(env, payload.headers);
    ERL_NIF_TERM body = wrap_string(env, payload.name);
    ERL_NIF_TERM schema = wrap_string(env, payload.name);
    return enif_make_tuple7(env, a_payload, name, description, parameters, headers, body, schema);
}

static ERL_NIF_TERM wrap_payloads(ErlNifEnv* env, const Collection<Payload>::type payloads) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Collection<Payload>::const_iterator it = payloads.begin(); it != payloads.end(); ++it) {
        list = enif_make_list_cell(env,wrap_payload(env,*it),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_example(ErlNifEnv* env, const TransactionExample example) {
    ERL_NIF_TERM name = wrap_string(env, example.name);
    ERL_NIF_TERM description = wrap_string(env, example.description);
    ERL_NIF_TERM requests = wrap_payloads(env, example.requests);
    ERL_NIF_TERM responses = wrap_payloads(env, example.responses);
    return enif_make_tuple5(env, a_transaction_example, name, description, requests, responses);
}

static ERL_NIF_TERM wrap_examples(ErlNifEnv* env, const Collection<TransactionExample>::type examples) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Collection<TransactionExample>::const_iterator it = examples.begin(); it != examples.end(); ++it) {
        list = enif_make_list_cell(env,wrap_example(env,*it),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_action(ErlNifEnv* env, const Action action) {
    ERL_NIF_TERM method = wrap_string(env, action.method);
    ERL_NIF_TERM name = wrap_string(env, action.name);
    ERL_NIF_TERM description = wrap_string(env, action.description);
    ERL_NIF_TERM parameters = wrap_parameters(env, action.parameters);
    ERL_NIF_TERM headers = wrap_kvpairs(env, action.headers);
    ERL_NIF_TERM examples = wrap_examples(env, action.examples);
    return enif_make_tuple7(env, a_action, method, name, description, parameters, headers, examples);
}

static ERL_NIF_TERM wrap_actions(ErlNifEnv* env, const Collection<Action>::type actions) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Collection<Action>::const_iterator it = actions.begin(); it != actions.end(); ++it) {
        list = enif_make_list_cell(env,wrap_action(env,*it),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_resource(ErlNifEnv* env, const Resource resource) {
    ERL_NIF_TERM uri_template = wrap_string(env, resource.uriTemplate);
    ERL_NIF_TERM name = wrap_string(env, resource.name);
    ERL_NIF_TERM description = wrap_string(env, resource.description);
    ERL_NIF_TERM model = wrap_payload(env, resource.model);
    ERL_NIF_TERM parameters = wrap_parameters(env, resource.parameters);
    ERL_NIF_TERM headers = wrap_kvpairs(env, resource.headers);
    ERL_NIF_TERM actions = wrap_actions(env, resource.actions);
    return enif_make_tuple8(env, a_resource, uri_template, name, description, model, parameters, headers, actions);
}

static ERL_NIF_TERM wrap_resources(ErlNifEnv* env, const Collection<Resource>::type resources) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Collection<Resource>::const_iterator it = resources.begin(); it != resources.end(); ++it) {
        list = enif_make_list_cell(env,wrap_resource(env,*it),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_resourceGroup(ErlNifEnv* env, const ResourceGroup resourceGroup) {
    ERL_NIF_TERM name = wrap_string(env, resourceGroup.name);
    ERL_NIF_TERM description = wrap_string(env, resourceGroup.description);
    return enif_make_tuple4(env, a_resource_group, name, description, wrap_resources(env, resourceGroup.resources));
}

static ERL_NIF_TERM wrap_resourceGroups(ErlNifEnv* env, const Collection<ResourceGroup>::type resourceGroups) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Collection<ResourceGroup>::const_iterator it = resourceGroups.begin(); it != resourceGroups.end(); ++it) {
        list = enif_make_list_cell(env,wrap_resourceGroup(env,*it),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_blueprint(ErlNifEnv* env, const Blueprint blueprint) {
    ERL_NIF_TERM name = wrap_string(env, blueprint.name);
    ERL_NIF_TERM description = wrap_string(env, blueprint.description);
    return enif_make_tuple5(env, a_blueprint, wrap_kvpairs(env, blueprint.metadata), name, description, wrap_resourceGroups(env, blueprint.resourceGroups));
}

static ERL_NIF_TERM parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    if (argc == 2 && enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
        std::string str ((const char*) bin.data, bin.size);
        BlueprintParserOptions options = 0;
        Result result;
        Blueprint blueprint;
        parse(str, options, result, blueprint);
        if (result.error.code == Error::OK) {
            ERL_NIF_TERM term;
            if (enif_compare(a_json, argv[0]) == 0) {
                std::stringstream ss;
                SerializeJSON(blueprint, ss);
                term = wrap_string(env, ss.str());
            } else if (enif_compare(a_yaml, argv[0]) == 0) {
                std::stringstream ss;
                SerializeYAML(blueprint, ss);
                term = wrap_string(env, ss.str());
            } else {
                term = wrap_blueprint(env, blueprint);
            }
            return enif_make_tuple3(env, a_ok, wrap_result(env,result), term);
        } else {
            return enif_make_tuple2(env, a_error, wrap_result(env,result));
        }
    } else {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {
    {"parse", 2, parse_nif}
};

ERL_NIF_INIT(raven,nif_funcs,&on_load,NULL,NULL,NULL)