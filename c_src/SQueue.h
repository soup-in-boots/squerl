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
        static ERL_NIF_TERM BlockPop(ErlNifEnv *caller, const std::string &name, const ERL_NIF_TERM &self);
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

        void push(ErlNifEnv *caller, const ERL_NIF_TERM &v) {
            Node *tmp = new Node( new Term(v) );

            enif_mutex_lock(tl_lock);

            Node *fb = first_blocker;
            Node *nb = fb->next.load(std::memory_order_acquire);

            if (nb != nullptr) {
                // Pop blocker; move blocker hd
                Term *blocker = nb->term;
                nb->term = nullptr;
                first_blocker = nb;
                enif_mutex_unlock(tl_lock);

                // Send message
                send_term(caller, blocker, tmp->term);

                // Release related values
                delete blocker;
                delete fb;
                delete tmp;
            } else {
                last_term->next = tmp;
                last_term = tmp;
                enif_mutex_unlock(tl_lock);
            }
        }

        void send_term(ErlNifEnv *caller, Term *blocker, Term *msg) {
            ErlNifPid target;
            enif_get_local_pid(blocker->env, blocker->term, &target);
            msg->term = enif_make_tuple2(msg->env, enif_make_atom(msg->env, "$squeue_bpop_response"), msg->term); 
            enif_send(caller, &target, msg->env, msg->term);
        }
        
        ERL_NIF_TERM pop(ErlNifEnv *caller) {
            ERL_NIF_TERM ret;

            enif_mutex_lock(hd_lock);

            Node* hd = first_term;
            Node* nx = first_term->next.load(std::memory_order_acquire);

            if (nx != nullptr) {
                Term* v = nx->term;
                nx->term = nullptr;
                first_term = nx;
                enif_mutex_unlock(hd_lock);
                
                ret = enif_make_copy(caller, v->term);
                delete(v);
                delete(hd);
            } else {
                enif_mutex_unlock(hd_lock);
                ret = enif_make_atom(caller, "empty");
            }

            return ret;
        }

        ERL_NIF_TERM block_pop(ErlNifEnv *caller, const ERL_NIF_TERM &self) {
            Node *tmp = new Node(new Term(self));
            ERL_NIF_TERM ret;

            enif_mutex_lock(hd_lock);

            Node *f = first_term;
            Node *n = first_term->next.load(std::memory_order_acquire);
            
            if (n != nullptr) {
                Term *v = n->term;
                n->term = nullptr;
                first_term = n;
                enif_mutex_unlock(hd_lock);

                ret = enif_make_tuple2(caller, enif_make_atom(caller, "$squeue_bpop_response"), enif_make_copy(caller, v->term));
                delete v;
                delete f;
                delete tmp;
            } else {
                last_blocker->next = tmp;
                last_blocker = tmp;
                enif_mutex_unlock(hd_lock);

                ret = enif_make_atom(caller, "$squeue_bpop_wait");
            }

            return ret;
        }

        Node *first_term;
        Node *last_term;

        Node *first_blocker;
        Node *last_blocker;

        ErlNifMutex *hd_lock;       // Protects hd(terms) and tl(blockers)
        ErlNifMutex *tl_lock;       // Protects tl(terms) and hd(blockers)
};

#endif
