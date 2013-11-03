#include <iostream>
#include <sstream>
#include <fstream>
#include "erl_nif.h"
#include "snowcrash.h"
#include "SerializeJSON.h"
#include "SerializeYAML.h"

using snowcrash::SourceAnnotation;
using snowcrash::Error;

// static variables
static ERL_NIF_TERM a_json;
static ERL_NIF_TERM a_yaml;
static ERL_NIF_TERM a_ok;
static ERL_NIF_TERM a_error;

static void init_atoms(ErlNifEnv* env) {

    a_json = enif_make_atom(env, "json");
    a_yaml = enif_make_atom(env, "yaml");

    a_ok = enif_make_atom(env, "ok");
    a_error = enif_make_atom(env, "error");

}

static int on_load(ErlNifEnv* env, void**, ERL_NIF_TERM) {
    init_atoms(env);
    return 0;
}

static ERL_NIF_TERM parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    if (enif_inspect_iolist_as_binary(env, argv[1], &bin)) {
        std::string str ((const char*) bin.data, bin.size);
        snowcrash::BlueprintParserOptions options = 0;
        snowcrash::Result result;
        snowcrash::Blueprint blueprint;
        snowcrash::parse(str, options, result, blueprint);
        if (result.error.code == Error::OK) {
            ERL_NIF_TERM res;
            if (enif_compare(a_json,argv[0]) == 0) {
                std::stringstream ss;
                SerializeJSON(blueprint, ss);
                res = enif_make_string(env,ss.str().c_str(),ERL_NIF_LATIN1);
            } else if (enif_compare(a_yaml,argv[0]) == 0) {
                std::stringstream ss;
                SerializeYAML(blueprint, ss);
                res = enif_make_string(env,ss.str().c_str(),ERL_NIF_LATIN1);
            } else {
                res = enif_make_string(env,str.c_str(),ERL_NIF_LATIN1);
            }
            return enif_make_tuple2(env,a_ok,res);
        } else {
            return enif_make_tuple2(env,a_error,enif_make_atom(env, "b"));
        }
    } else {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {
    {"parse", 2, parse_nif}
};

ERL_NIF_INIT(raven,nif_funcs,&on_load,NULL,NULL,NULL)