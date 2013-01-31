#include "SQueue.h"

std::unordered_map<std::string, SQueue*> SQueue::queues;
ErlNifRWLock *SQueue::queue_lock;

void SQueue::Initialize() {
    queue_lock = enif_rwlock_create("shared_queue_lock");
}

void SQueue::Shutdown() {
    enif_rwlock_destroy(queue_lock);
}

SQueue::SQueue() : first_term(NULL), last_term(NULL), hd_lock(NULL), tl_lock(NULL) {
    first_term = last_term = new Node();
    first_blocker = last_blocker = new Node();
    hd_lock = enif_mutex_create("squeue_hd_lock");
    tl_lock = enif_mutex_create("squeue_tl_lock");
}

SQueue::~SQueue() {
    Node *it = first_term;
    Node *tmp = nullptr;
    while (it != nullptr) {
        tmp = it;
        it = it->next;
        delete tmp;
    }

    it = first_blocker;
    tmp = nullptr;
    while (it != nullptr) {
        tmp = it;
        it = it->next;
        delete tmp;
    }

    enif_mutex_destroy(hd_lock);
    enif_mutex_destroy(tl_lock);
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

    queue = GetQueue(name);
    if (queue == NULL) {
        return enif_make_badarg(caller);
    }
    
    queue->push(caller, value);

    return enif_make_atom(caller, "ok");
}

ERL_NIF_TERM SQueue::Pop(ErlNifEnv *caller, const std::string &name) {
    SQueue *queue;
    ERL_NIF_TERM ret;

    queue = GetQueue(name);
    if (queue == NULL) {
        return enif_make_badarg(caller);
    }

    ret = queue->pop(caller);

    return ret;
}

ERL_NIF_TERM SQueue::BlockPop(ErlNifEnv *caller, const std::string &name, const ERL_NIF_TERM &self) {
    SQueue *queue;
    ERL_NIF_TERM ret;
    
    queue = GetQueue(name);
    if (queue == NULL) {
        return enif_make_badarg(caller);
    }

    ret = queue->block_pop(caller, self);

    return ret;
}
