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

// static variables
static ERL_NIF_TERM a_json;
static ERL_NIF_TERM a_yaml;
static ERL_NIF_TERM a_ok;
static ERL_NIF_TERM a_error;
static ERL_NIF_TERM a_not_implemented;

static ERL_NIF_TERM a_result;
static ERL_NIF_TERM a_blueprint;
static ERL_NIF_TERM a_source_annotation;

static void init_atoms(ErlNifEnv* env) {

    a_json = enif_make_atom(env, "json");
    a_yaml = enif_make_atom(env, "yaml");

    a_ok = enif_make_atom(env, "ok");
    a_error = enif_make_atom(env, "error");
    a_not_implemented = enif_make_atom(env, "not_implemented");

    a_result = enif_make_atom(env, "result");
    a_blueprint = enif_make_atom(env, "blueprint");
    a_source_annotation = enif_make_atom(env, "source_annotation");

}

static int on_load(ErlNifEnv* env, void**, ERL_NIF_TERM) {
    init_atoms(env);
    return 0;
}

static ERL_NIF_TERM wrap_error(ErlNifEnv* env, Error error) {
    return a_not_implemented;
}

static ERL_NIF_TERM wrap_annotation(ErlNifEnv* env, const SourceAnnotation& annotation, const char *dict[]) {
    ERL_NIF_TERM code = enif_make_int(env, annotation.code);
    ERL_NIF_TERM type = enif_make_atom(env, dict[annotation.code]);
    ERL_NIF_TERM message = enif_make_string(env, annotation.message.empty() ? "" : annotation.message.c_str(), ERL_NIF_LATIN1);
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

static ERL_NIF_TERM wrap_warnings(ErlNifEnv* env, Warnings warnings) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (Warnings::const_iterator it = warnings.begin(); it != warnings.end(); ++it) {
        list = enif_make_list_cell(env,wrap_annotation(env,*it,warning_str),list);
    }
    return list;
}

static ERL_NIF_TERM wrap_result(ErlNifEnv* env, Result result) {
    return enif_make_tuple3(env, a_result, wrap_annotation(env,result.error,error_str), wrap_warnings(env,result.warnings));
}

static ERL_NIF_TERM wrap_blueprint(ErlNifEnv* env, Blueprint blueprint) {
    return a_not_implemented;
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
                term = enif_make_string(env, ss.str().c_str(), ERL_NIF_LATIN1);
            } else if (enif_compare(a_yaml, argv[0]) == 0) {
                std::stringstream ss;
                SerializeYAML(blueprint, ss);
                term = enif_make_string(env, ss.str().c_str(), ERL_NIF_LATIN1);
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