#ifndef SHAREDQUEUE_H
#define SHAREDQUEUE_H 1

#include "erl_nif.h"
#include <deque>
#include <string>
#include <unordered_map>
#include <atomic>
#include <time.h>
#include <iostream>
#include "squeue_utils.h"

#define GC_THRESHOLD 51200

typedef std::deque<ERL_NIF_TERM> TermQueue;

class SQueue {
    public:
        static void Initialize();
        static void Shutdown();
        static void* GarbageCollector(void *data);

        static ERL_NIF_TERM New(ErlNifEnv *caller, const std::string &name);

//        static ERL_NIF_TERM BlockPopFront(ErlNifEnv *caller, const std::string &name, const ERL_NIF_TERM &pid);
//        static ERL_NIF_TERM BlockBackPop(ErlNifEnv *caller, const std::string &name, const ERL_NIF_TERM &pid);
//        static ERL_NIF_TERM PushFront(ErlNifEnv *caller, const std::string &name, const ERL_NIF_TERM &value);

        static ERL_NIF_TERM Pop(ErlNifEnv *caller, const std::string &name);
        static ERL_NIF_TERM Push(ErlNifEnv *caller, const std::string &name, ERL_NIF_TERM value);

        static SQueue* GetQueue(const std::string &name);

    protected:
        static std::unordered_map<std::string, SQueue*> queues;
        static ErlNifRWLock *queue_lock;
        static ErlNifTid gc_tid;
        static std::atomic_bool running;

        SQueue();
        ~SQueue();

        void gc();

        ErlNifEnv *env;
        ErlNifRWLock *lock;
        ERL_NIF_TERM reclaim;
        unsigned long garbage_bytes;
        TermQueue blockers;
        TermQueue contents;
};

#endif
