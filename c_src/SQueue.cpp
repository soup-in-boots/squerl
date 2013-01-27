#include "SQueue.h"

std::unordered_map<std::string, SQueue*> SQueue::queues;
ErlNifRWLock *SQueue::queue_lock;
ErlNifTid SQueue::gc_tid;
std::atomic_bool SQueue::running;

void SQueue::Initialize() {
    queue_lock = enif_rwlock_create("shared_queue_lock");
    enif_thread_create("sharedqueue_gc", &gc_tid, &SQueue::GarbageCollector, NULL, NULL);
}

void SQueue::Shutdown() {
    running = false;
    enif_thread_join(gc_tid, NULL);
    enif_rwlock_destroy(queue_lock);
}

void *SQueue::GarbageCollector(void *data) {
    const timespec delay = {0, 50000};
    std::unordered_map<std::string, SQueue*>::iterator it;
    while (running) {
        nanosleep(&delay, NULL);
        
        enif_rwlock_rlock(queue_lock);
        for (it = queues.begin(); it != queues.end(); ++it) {
            it->second->gc();
        }
        enif_rwlock_runlock(queue_lock);
    }
    return NULL;
}

SQueue::SQueue() {
    env     = enif_alloc_env();
    lock    = enif_rwlock_create("shared_queue_lock");
    reclaim = enif_make_list(env, 0);
}

SQueue::~SQueue() {
    enif_rwlock_destroy(lock);
    enif_free_env(env);
}

void SQueue::gc() {
    unsigned long int size = 0;

    enif_rwlock_rwlock(lock);

    size = estimate_size(env, reclaim);
    reclaim = enif_make_list(env, 0);

    if (size >= GC_THRESHOLD) {
        ErlNifEnv *fresh = enif_alloc_env();
        TermQueue::iterator it;
        
        for (it = contents.begin(); it != contents.end(); ++it) {
            (*it) = enif_make_copy(fresh, (*it));
        }

        for (it = blockers.begin(); it != blockers.end(); ++it) {
            (*it) = enif_make_copy(fresh, (*it));
        }

        enif_free_env(env);
        env = fresh;
    }

    enif_rwlock_rwunlock(lock);
}

ERL_NIF_TERM SQueue::New(ErlNifEnv *caller, const std::string &name) {
    ERL_NIF_TERM ret;

    enif_rwlock_rwlock(queue_lock);
    if (queues.find(name) != queues.end()) {
        ret = enif_make_badarg(caller);
    } else {
        ret = enif_make_atom(caller, "ok");
        queues[name] = new SQueue();
    }
    enif_rwlock_rwunlock(queue_lock);

    return ret;
}

SQueue* SQueue::GetQueue(const std::string &name) {
    SQueue *ret = NULL;
    std::unordered_map<std::string, SQueue*>::iterator it;

    enif_rwlock_rlock(queue_lock);
    it = queues.find(name);
    if (it != queues.end()) {
        ret = it->second;
    }
    enif_rwlock_runlock(queue_lock);

    return ret;
}

ERL_NIF_TERM SQueue::Push(ErlNifEnv *caller, const std::string &name, ERL_NIF_TERM value) {
    SQueue *queue;
    ERL_NIF_TERM to_term;

    queue = GetQueue(name);
    if (queue == NULL) {
        return enif_make_badarg(caller);
    }

    enif_rwlock_rwlock(queue->lock);
    if (queue->blockers.empty()) {
        queue->contents.push_back(enif_make_copy(queue->env, value));
    } else {
        ErlNifPid pid;
        bool is_safe_pid = false;

        do {
            to_term = queue->blockers.front();
            queue->blockers.pop_front();
            queue->reclaim = enif_make_list_cell(queue->env, to_term, queue->reclaim);
            is_safe_pid = !(enif_is_pid(queue->env, to_term) && enif_get_local_pid(queue->env, to_term, &pid));
        } while (!queue->blockers.empty() && !is_safe_pid);

        if (is_safe_pid) {
            ErlNifEnv *msg_env = enif_alloc_env();
            enif_send(caller, &pid, msg_env, enif_make_tuple2(msg_env, enif_make_atom(msg_env, "$shared_queue_block_pop"), value));
            enif_free_env(msg_env);
        } else {
            queue->contents.push_back(enif_make_copy(queue->env, value));
        }
    }
    enif_rwlock_rwunlock(queue->lock);

    return enif_make_atom(caller, "ok");
}

ERL_NIF_TERM SQueue::Pop(ErlNifEnv *caller, const std::string &name) {
    SQueue *queue;
    ERL_NIF_TERM ret;

    queue = GetQueue(name);
    if (queue == NULL) {
        return enif_make_badarg(caller);
    }

    enif_rwlock_rwlock(queue->lock);
    if (!queue->contents.empty()) {
        ret = enif_make_tuple2(caller, enif_make_atom(caller, "ok"), enif_make_copy(caller, queue->contents.front()));
        queue->reclaim = enif_make_list_cell(queue->env, queue->contents.front(), queue->reclaim);
        queue->contents.pop_front();
    } else {
        ret = enif_make_atom(caller, "empty");
    }
    enif_rwlock_rwunlock(queue->lock);

    return ret;
}
