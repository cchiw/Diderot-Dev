/*! \file worker-gate.hxx
 *
 * \author John Reppy
 */

/*
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 */

#ifndef _DIDEROT_WORKER_GATE_HXX_
#define _DIDEROT_WORKER_GATE_HXX_

#ifdef DIDEROT_ENABLE_LOGGING
#include "logging.hxx"
#define IF_LOGGING(...)         __VA_ARGS__
#else
#define IF_LOGGING(...)
#endif

namespace diderot {

  //! this struct implements a two-step barrier synchronization.  First the controller
  //! thead waits until all of the workers are waiting at the "gate", then the controller_wait()
  //! can signal the workers to go (i.e., open the gate).  It is different from a regular
  //! barrier in that the controller is woken once all threads (workers and controllers)
  //! are blocked and can then run code before releasing the workers.
    struct CACHE_ALIGN worker_gate {
        uint32_t        _nWorkers;      //!< number of workers in the pool
        uint32_t        _nWaiting;      //!< number of workers waiting
        pthread_mutex_t _lock;          //!< lock to protect this struct
        pthread_cond_t  _waiting;       //!< used by workers to worker_gate that they are waiting
        pthread_cond_t  _go;            //!< used by workers to worker_gate termination

        worker_gate ()
            : _nWaiting(0), _nWorkers(0)
        {
            pthread_mutex_init (&this->_lock, nullptr);
            pthread_cond_init (&this->_waiting, nullptr);
            pthread_cond_init (&this->_go, nullptr);
        }

        ~worker_gate ()
        {
            pthread_mutex_destroy (&this->_lock);
            pthread_cond_destroy (&this->_waiting);
            pthread_cond_destroy (&this->_go);
        }

      // initialize the gate for the given number of workers
        void init (uint32_t nw) { this->_nWorkers = nw; }

      // workers call this function to wait for "go" signal from controller
        void worker_wait (IF_LOGGING( struct world_base *wrld, int32_t id ))
        {
            IF_LOGGING( LogWorkerGateWait(wrld, id+1); )
            pthread_mutex_lock (&this->_lock);
                this->_nWaiting++;
                if (this->_nWaiting == this->_nWorkers) {
                    pthread_cond_signal (&this->_waiting);
                }
                pthread_cond_wait (&this->_go, &this->_lock);
            pthread_mutex_unlock (&this->_lock);
            IF_LOGGING( LogWorkerGateResume(wrld, id+1); )
        }

      // the controller calls this function to wait until all the workers are
      // at the gate
        void controller_wait (IF_LOGGING( struct world_base *wrld ))
        {
            IF_LOGGING( LogControllerGateWait(wrld, 0); )
            pthread_mutex_lock (&this->_lock);
                if (this->_nWaiting < this->_nWorkers) {
                    pthread_cond_wait (&this->_waiting, &this->_lock);
                }
                this->_nWaiting = 0;
            pthread_mutex_unlock (&this->_lock);
            IF_LOGGING( LogControllerGateResume(wrld, 0); )
        }

      // wait until all workers are waiting and then signal them to go
        void release_workers (IF_LOGGING( struct world_base *wrld ))
        {
            IF_LOGGING( LogGateReleaseWorkers(wrld, 0); )
            pthread_cond_broadcast (&this->_go);
        }

    }; // struct worker_gate

} // namespace diderot

#undef IF_LOGGING

#endif // !_DIDEROT_WORKER_GATE_HXX_
