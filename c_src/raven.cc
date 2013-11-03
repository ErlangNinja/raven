#include <iostream>
#include <sstream>
#include <fstream>
#include "erl_nif.h"
#include "snowcrash.h"

using snowcrash::SourceAnnotation;
using snowcrash::Error;

extern int parse(int i);

/// \brief Print Markdown source annotation.
/// \param prefix A string prefix for the annotation
/// \param annotation An annotation to print
void PrintAnnotation(const std::string& prefix, const snowcrash::SourceAnnotation& annotation)
{
    std::cerr << prefix;
    
    if (annotation.code != SourceAnnotation::OK) {
        std::cerr << " (" << annotation.code << ") ";
    }
    
    if (!annotation.message.empty()) {
        std::cerr << " " << annotation.message;
    }
    
    if (!annotation.location.empty()) {
        for (snowcrash::SourceCharactersBlock::const_iterator it = annotation.location.begin();
             it != annotation.location.end();
             ++it) {
            std::cerr << ((it == annotation.location.begin()) ? " :" : ";");
            std::cerr << it->location << ":" << it->length;
        }
    }
    
    std::cerr << std::endl;
}

/// \brief Print parser result to stderr.
/// \param result A parser result to print
void PrintResult(const snowcrash::Result& result)
{
    std::cerr << std::endl;
    
    if (result.error.code == Error::OK) {
        std::cerr << "OK.\n";
    }
    else {
        PrintAnnotation("error:", result.error);
    }
    
    for (snowcrash::Warnings::const_iterator it = result.warnings.begin(); it != result.warnings.end(); ++it) {
        PrintAnnotation("warning:", *it);
    }
}

static ERL_NIF_TERM parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int length;
    char* buffer = NULL;

    if (!enif_get_list_length(env, argv[0], &length)) {
        return enif_make_badarg(env);
    }
    
    length++;

    buffer = (char*) malloc(length * sizeof(char));
    if (enif_get_string(env, argv[0], buffer, length+1, ERL_NIF_LATIN1) < 1) {
        free(buffer);
        return enif_make_badarg(env);
    }

    // Parse
    snowcrash::BlueprintParserOptions options = 0;
    snowcrash::Result result;
    snowcrash::Blueprint blueprint;
    snowcrash::parse(buffer, options, result, blueprint);

    PrintResult(result);

    free(buffer);

    return enif_make_int(env, 0);
}

static ErlNifFunc nif_funcs[] =
{
    {"parse", 1, parse_nif}
};

ERL_NIF_INIT(snowcrash,nif_funcs,NULL,NULL,NULL,NULL)