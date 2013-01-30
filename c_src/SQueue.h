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

#define GC_THRESHOLD 1048576 * 10

// CACHE_LINE_SIZE is a terrible assumption and should be fixed
// Compiler-time definition would be ideal?
#define CACHE_LINE_SIZE 64

class SQueue {
    public:
        static void Initialize();
        static void Shutdown();

        static ERL_NIF_TERM New(ErlNifEnv *caller, const std::string &name);

//        static ERL_NIF_TERM BlockPopFront(ErlNifEnv *caller, const std::string &name, const ERL_NIF_TERM &pid);
//        static ERL_NIF_TERM BlockBackPop(ErlNifEnv *caller, const std::string &name, const ERL_NIF_TERM &pid);
//        static ERL_NIF_TERM PushFront(ErlNifEnv *caller, const std::string &name, const ERL_NIF_TERM &value);

        static ERL_NIF_TERM Pop(ErlNifEnv *caller, const std::string &name);
        static ERL_NIF_TERM Push(ErlNifEnv *caller, const std::string &name, ERL_NIF_TERM value);

        static SQueue* GetQueue(const std::string &name);

    protected:
        struct Term {
            Term(ERL_NIF_TERM v) { 
                env = enif_alloc_env();
                term = enif_make_copy(env, v);
            }
            ~Term() {
                enif_free_env(env);
            }
            ErlNifEnv *env;
            ERL_NIF_TERM term;
        };
        struct Node {
            Node() : term(nullptr), next(nullptr) {}
            Node(Term *v) : term(v), next(nullptr)  {}
            Term *term;
            std::atomic<Node*> next;
            char pad[CACHE_LINE_SIZE - sizeof(ERL_NIF_TERM*) - sizeof(std::atomic<Node*>)];
        };
        static ErlNifRWLock *queue_lock;
        static std::unordered_map<std::string, SQueue*> queues;
        static std::atomic_bool running;

        SQueue();
        ~SQueue();

        void push(const ERL_NIF_TERM &v) {
            Node *tmp = new Node( new Term(v) );
            enif_mutex_lock(tl_lock);
            last->next = tmp;
            last = tmp;
            enif_mutex_unlock(tl_lock);
        }
        
        ERL_NIF_TERM pop(ErlNifEnv *dest) {
            ERL_NIF_TERM ret;

            enif_mutex_lock(hd_lock);

            Node* hd = first;
            Node* nx = first->next;

            if (nx != nullptr) {
                Term* v = nx->term;
                nx->term = nullptr;
                first = nx;
                enif_mutex_unlock(hd_lock);
                
                ret = enif_make_copy(dest, v->term);
                delete(v);
                delete(hd);
            } else {
                enif_mutex_unlock(hd_lock);
                ret = enif_make_atom(dest, "empty");
            }

            return ret;
        }

        Node *first;
        Node *last;

        ErlNifMutex *hd_lock;
        ErlNifMutex *tl_lock;
};

#endif
