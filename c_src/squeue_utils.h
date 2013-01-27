#ifndef SQUEUE_UTILS_H
#define SQUEUE_UTILS_H

#include "erl_nif.h"
#define WORD_SIZE sizeof(int)

unsigned long int estimate_size(ErlNifEnv *env, ERL_NIF_TERM term);

#endif
