#include "erl_nif.h"
#include "SQueue.h"
#include <iostream>

static ERL_NIF_TERM squeue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM squeue_push(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM squeue_pop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM squeue_block_pop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"new", 1, squeue_new},
    {"push", 2, squeue_push},
    {"pop", 1, squeue_pop},
    {"do_bpop", 2, squeue_block_pop},
};

static ERL_NIF_TERM squeue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char *name = NULL;
    unsigned int size = 0;
    ERL_NIF_TERM ret;

    if (!enif_is_atom(env, argv[0])) {
        return enif_make_badarg(env);
    }

    enif_get_atom_length(env, argv[0], &size, ERL_NIF_LATIN1);
    name = (char*)enif_alloc(sizeof(char) * (size + 1));
    enif_get_atom(env, argv[0], name, size + 1, ERL_NIF_LATIN1);

    ret = SQueue::New(env, name);
    enif_free(name);

    return ret;
}

static ERL_NIF_TERM squeue_push(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    char *name = NULL;
    unsigned int size = 0;
    ERL_NIF_TERM ret;

    if (!enif_is_atom(env, argv[0])) {
        return enif_make_badarg(env);
    }

    enif_get_atom_length(env, argv[0], &size, ERL_NIF_LATIN1);
    name = (char*)enif_alloc(sizeof(char) * (size + 1));
    enif_get_atom(env, argv[0], name, size + 1, ERL_NIF_LATIN1);

    ret = SQueue::Push(env, name, argv[1]);
    enif_free(name);

    return ret;
}

static ERL_NIF_TERM squeue_pop(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    char *name = NULL;
    unsigned int size = 0;
    ERL_NIF_TERM ret;

    if (!enif_is_atom(env, argv[0])) {
        return enif_make_badarg(env);
    }

    enif_get_atom_length(env, argv[0], &size, ERL_NIF_LATIN1);
    name = (char*)enif_alloc(sizeof(char) * (size + 1));
    enif_get_atom(env, argv[0], name, size + 1, ERL_NIF_LATIN1);

    ret = SQueue::Pop(env, name);
    enif_free(name);

    return ret;
}

static ERL_NIF_TERM squeue_block_pop(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    char *name = NULL;
    unsigned int size = 0;
    ERL_NIF_TERM ret;
    ErlNifPid self;

    if (!enif_is_atom(env, argv[0])) {
        return enif_make_badarg(env);
    }

    if (!enif_get_local_pid(env, argv[1], &self)) {
        return enif_make_badarg(env);
    }

    enif_get_atom_length(env, argv[0], &size, ERL_NIF_LATIN1);
    name = (char*)enif_alloc(sizeof(char) * (size + 1));
    enif_get_atom(env, argv[0], name, size + 1, ERL_NIF_LATIN1);

    ret = SQueue::BlockPop(env, name, argv[1]);
    enif_free(name);
    
    return ret;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    SQueue::Initialize();
    return 0;
}

static void on_unload(ErlNifEnv *env, void *data) {
    SQueue::Shutdown();
}

ERL_NIF_INIT(squeue, nif_funcs, &on_load, NULL, NULL, &on_unload);
