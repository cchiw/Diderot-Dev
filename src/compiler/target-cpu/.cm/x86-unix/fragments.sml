110.81  x86    
            @       Q     �  �5;�0erho�f�V      <R��RѪ)(	p�y<R��RѪ)(	p�y               n               n�5;�0erho�f�guid-driver/(sources.cm):../target-cpu/(sources.cm):fragments.sml-1522024067.942
 ��    �"     �/*---------- begin c-wrappers.in ----------*/
extern "C" uint32_t @PREFIX@_num_strands (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    return w->_strands.num_alive();
}

extern "C" uint32_t @PREFIX@_num_active_strands (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    return w->_strands.num_active();
}

extern "C" uint32_t @PREFIX@_num_stable_strands (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    return w->_strands.num_stable();
}

extern "C" @BOOLTY@ @PREFIX@_any_errors (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    return (w->_errors->errNum > 0);
}

extern "C" char *@PREFIX@_get_errors (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    char *msg = biffMsgStrGet (w->_errors);
    biffMsgClear (w->_errors);
    return msg;
}

extern "C" @PREFIX@_world_t *@PREFIX@_new_world ()
{
    @PREFIX@::world *w = new (std::nothrow) @PREFIX@::world();
    return reinterpret_cast<@PREFIX@_world_t *>(w);
}

extern "C" @BOOLTY@ @PREFIX@_init_world (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);

    if (w->_stage != diderot::POST_NEW) {
        w->error ("multiple calls to @PREFIX@_init_world");
        return true;
    }

    if (w->init()) {
        return true;
    }

#ifndef DIDEROT_NO_INPUTS
    if (w != nullptr) {
        init_defined_inputs (w);
        init_defaults (w->_globals);
    }
#endif

    return false;
}

extern "C" @BOOLTY@ @PREFIX@_create_strands (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);

    if (w->_stage < diderot::POST_INIT) {
        w->error ("must call @PREFIX@_init_world before @PREFIX@_create_strands");
        return true;
    }
    else if (w->_stage > diderot::POST_INIT) {
        w->error ("multiple calls to @PREFIX@_create_strands");
        return true;
    }

#ifndef DIDEROT_NO_INPUTS
    if (check_defined(w)) {
        return true;
    }
#endif
    return static_cast<@BOOLTY@>(w->create_strands());
}

extern "C" uint32_t @PREFIX@_run (@PREFIX@_world_t *wrld, uint32_t maxNSteps)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);

    if (w->_stage < diderot::POST_CREATE) {
        w->error ("attempt to run uninitialized program");
        return 0;
    }
    else if (w->_stage == diderot::DONE) {
        return 0;
    }

    return w->run(maxNSteps);
}

extern "C" void @PREFIX@_shutdown (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    delete w;
}

extern "C" void @PREFIX@_set_verbose (@PREFIX@_world_t *wrld, @BOOLTY@ mode)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    w->_verbose = (mode ? true : false);
}

extern "C" @BOOLTY@ @PREFIX@_get_verbose (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    return static_cast<@BOOLTY@>(w->_verbose);
}

#ifdef DIDEROT_TARGET_PARALLEL

bool @PREFIX@_set_num_workers (@PREFIX@_world_t *wrld, uint32_t nw)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    if (w->_sched->_numHWCores < nw) {
        w->_sched->_numWorkers = w->_sched->_numHWCores;
        return true;
    }
    else if (nw > 0) {
        w->_sched->_numWorkers = nw;
    }
    else {
        w->_sched->_numWorkers = w->_sched->_numHWCores;
    }
    return false;
}

uint32_t @PREFIX@_get_num_workers (@PREFIX@_world_t *wrld)
{
    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);
    return w->_sched->_numWorkers;
}

#endif /* DIDEROT_TARGET_PARALLEL */
/*---------- end c-wrappers.in ----------*/
  	/*---------- begin par-main.in ----------*/
using namespace @PREFIX@;

//! Main function for standalone parallel C target
//
int main (int argc, const char **argv)
{
    bool        timingFlg = false;      //! true if timing computation
    uint32_t    stepLimit = 0;          //! limit on number of execution steps (0 means unlimited)
    std::string printFile = "-";        //! file to direct printed output into
    uint32_t    reqNumWorkers;          //! requested number of worker threads
#ifdef DIDEROT_EXEC_SNAPSHOT
    uint32_t    snapshotPeriod = 1;     //! supersteps per snapshot
#endif
    uint32_t    nSteps = 0;             //! number of supersteps taken

  // create the world
    world *wrld = new (std::nothrow) world();
    if (wrld == nullptr) {
        std::cerr << "unable to create world" << std::endl;
        exit (1);
    }

  // initialize scheduler stuff
    if (wrld->_verbose) {
        std::cerr << "CPU info: " << wrld->_sched->_numHWCores << " cores / "
            << wrld->_sched->_numHWThreads << " threads\n";
        std::cerr << "initializing world ..." << std::endl;
    }
    if (wrld->init()) {
        std::cerr << "Error initializing world:\n" << wrld->get_errors() << std::endl;
        delete wrld;
        exit(1);
    }

#ifndef DIDEROT_NO_INPUTS
  // initialize the default values for the inputs
    cmd_line_inputs inputs;
    init_defaults (&inputs);
#endif

  // handle command-line options
    {
        diderot::options *opts = new diderot::options ();
        reqNumWorkers = wrld->_sched->_numHWCores;
        opts->add ("l,limit", "specify limit on number of super-steps (0 means unlimited)",
            &stepLimit, true);
#ifdef DIDEROT_EXEC_SNAPSHOT
        opts->add ("s,snapshot",
            "specify number of super-steps per snapshot (0 means no snapshots)",
            &snapshotPeriod, true);
#endif
        opts->add ("print", "specify where to direct printed output", &printFile, true);
        opts->addFlag ("v,verbose", "enable runtime-system messages", &(wrld->_verbose));
        opts->addFlag ("t,timing", "enable execution timing", &timingFlg);
        opts->add ("n,nworkers", "specify number of worker threads", &reqNumWorkers, true);
#ifndef DIDEROT_NO_INPUTS
      // register options for setting global inputs
        register_inputs (&inputs, opts);
#endif
        register_outputs (opts);
        opts->process (argc, argv);
        delete opts;
    }

  // redirect printing (if necessary)
    if (printFile.compare("-") != 0) {
        wrld->_printTo = new std::ofstream (printFile);
        if (wrld->_printTo->fail()) {
            std::cerr << "Error opening print file" << std::endl;
            delete wrld;
            exit(1);
        }
    }

    wrld->_sched->set_num_workers (reqNumWorkers);
#ifdef DIDEROT_ENABLE_LOGGING
  // initialize logging
    wrld->_log_file = new diderot::log::file("@LOG_FILE@", wrld->_sched);
#endif
    if (wrld->_sched->create_workers (wrld)) {
        std::cerr << "Error creating workers:\n" << wrld->get_errors() << std::endl;
        delete wrld;
        exit(1);
    }

#ifndef DIDEROT_NO_INPUTS
  // initialize the input globals
    if (init_inputs (wrld, &inputs)) {
        std::cerr << "Error initializing inputs:\n" << wrld->get_errors() << std::endl;
        delete wrld;
        exit(1);
    }
#endif

  // run the generated global initialization code
    if (wrld->_verbose) {
        std::cerr << "initializing globals and creating strands ...\n";
    }
    if (wrld->create_strands()) {
        std::cerr << "Error in global initialization:\n"
            << wrld->get_errors() << std::endl;
        delete wrld;
        exit(1);
    }

#ifdef DIDEROT_EXEC_SNAPSHOT

    if (snapshotPeriod > 0) {
     // write initial state as snapshot 0
        write_snapshot (wrld, "-0000");
     // run the program for `snapshotPeriod` steps at a time with a snapshot after each run
        while (true) {
            uint32_t n, limit;
          // determine a step limit for the next run
            if (stepLimit > 0) {
                if (stepLimit <= nSteps) {
                    break;
                }
                limit = std::min(stepLimit - nSteps, snapshotPeriod);
            }
            else {
                limit = snapshotPeriod;
            }
          // run the program for upto limit steps
            if ((n = wrld->run (limit)) == 0) {
                break;
            }
            nSteps += n;
            if ((wrld->_errors->errNum > 0) || (wrld->_strands.num_alive() == 0)) {
                break;
            }
          // write a snapshot with the step count as a suffix
            std::string suffix = std::to_string(nSteps);
            if (suffix.length() < 4) {
                suffix = std::string("0000").substr(0, 4 - suffix.length()) + suffix;
            }
            suffix = "-" + suffix;
            write_snapshot (wrld, suffix);
        }
    }
    else {
        nSteps = wrld->run (stepLimit);
    }

#else // !DIDEROT_EXEC_SNAPSHOT

    nSteps = wrld->run (stepLimit);

#endif // DIDEROT_EXEC_SNAPSHOT

  // shutdown the workers
    wrld->_sched->shutdown (wrld);

    if (wrld->_errors->errNum > 0) {
        std::cerr << "Error during execution:\n" << wrld->get_errors() << std::endl;
        delete wrld;
        exit(1);
    }

    if ((stepLimit != 0) && (wrld->_strands.num_active() > 0)) {
#ifdef DIDEROT_STRAND_ARRAY
        if (wrld->_verbose) {
            std::cerr << "Step limit expired; "
                << wrld->_strands.num_active() << " active strands remaining" << std::endl;
        }
#else
      // step limit expired, so kill remaining strands
        if (wrld->_verbose) {
            std::cerr << "Step limit expired. Killing remaining "
                << wrld->_strands.num_active() << " active strands" << std::endl;
        }
        wrld->kill_all();
#endif
    }

    if (wrld->_verbose) {
        std::cerr << "done: " << nSteps << " steps, in " << wrld->_run_time << " seconds";
#ifndef DIDEROT_STRAND_ARRAY
        std::cerr << "; " << wrld->_strands.num_stable() << " stable strands" << std::endl;
#else
        std::cerr << std::endl;
#endif
    }
    else if (timingFlg) {
        std::cout << "usr=" << wrld->_run_time << std::endl;
    }

  // output the final strand states
    write_output (wrld);

    delete wrld;

    return 0;

} // main
/*---------- end par-main.in ----------*/
  �/*---------- begin par-run.in ----------*/
//! Run the Diderot program (parallel version)
//! \param maxNSteps the limit on the number of super steps; 0 means unlimited
//! \return the number of steps taken, or 0 on error.
uint32_t world::run (uint32_t maxNSteps)
{
    if (this->_stage == diderot::POST_CREATE) {
#ifdef DIDEROT_HAS_GLOBAL_START
        this->global_start();
#endif
        this->_stage = diderot::RUNNING;
    }
    assert (this->_stage == diderot::RUNNING);

    diderot::scheduler *sched = this->_sched;

    if (maxNSteps == 0) {
        maxNSteps = 0xffffffff;  // essentially unlimited
    }

  // set task pointer
    sched->_task = worker;

  // initialize per-worker info
    this->_strands._workers.clear();
    worker_arg *args = new worker_arg[sched->_numWorkers];
    for (int i = 0;  i < sched->_numWorkers;  i++) {
        worker_arg *p = &args[i];
        p->_wrld = this;
        p->_id = i;
        p->_maxNSteps = maxNSteps;
        p->_nSteps = 0;
#ifndef DIDEROT_BSP
        p->_nStable = 0;
        p->_nDead = 0;
#endif
        p->_strands.init (this->_strands);
        sched->_info[i]._data = p;
    }

    double t0 = airTime();

  // Start worker threads
    if (this->_verbose) {
        std::cerr << "run with " << this->_strands.num_active() << " active strands / "
            << sched->_numWorkers << " workers ..." << std::endl;
    }
    this->_strands.prepare_run ();
    sched->_gate.release_workers (IF_LOGGING( this ));

  // wait for the computation to finish
    sched->_gate.controller_wait (IF_LOGGING( this ));

  // get max # steps and update global counts of active and stable strands when no-bsp
    uint32_t nSteps = 0;
    for (uint32_t i = 0;  i < sched->_numWorkers;  i++) {
        nSteps = std::max (nSteps, args[i]._nSteps);
#ifndef DIDEROT_BSP
      // if there is no BSP, then the controller updates #active and #stable
        this->_strands._nActive -= args[i]._nStable + args[i]._nDead;
        this->_strands._nStable += args[i]._nStable;
#endif
    }
    delete[] args;

    t0 = airTime() - t0;
    if (this->_verbose) {
        std::cerr << nSteps << " steps done in " << t0 << " seconds" << std::endl;
    }
    this->_run_time += t0;

    return nSteps;

} // world::run
/*---------- end par-run.in ----------*/
  /*---------- begin par-run-start.in ----------*/
// Run the start methods of the initial strands (parallel version)
//
void worker_cache::run_start_methods (@START_PARAMS@sched_block *bp)
{
    for (auto ix = this->begin_fresh(bp); ix != this->end_fresh(bp); )
    {
        diderot::strand_status sts = this->strand_start(@START_ARGS@ix);
        switch (sts) {
          case diderot::kStabilize:
            ix = this->strand_stabilize (bp, @STABILIZE_ARGS@ix);
            break;
#ifdef DIDEROT_HAS_STRAND_DIE
          case diderot::kDie:
            ix = this->kill (bp, ix);
            break;
#endif
          default:
	    this->_status[ix] = diderot::kActive;
            ix = this->next_fresh(bp, ix);
            break;
        }
    }

}
/*---------- end par-run-start.in ----------*/
  �/*---------- begin par-worker-nobsp.in ----------*/
struct CACHE_ALIGN worker_arg {
    world       *_wrld;         //!< world pointer
    uint32_t    _id;            //!< worker ID
    uint32_t    _maxNSteps;     //!< maximum number of steps to take; 0 == infinity
    uint32_t    _nSteps;        //!< max number of steps taken by a strand in call to run
    uint32_t    _nStable;       //!< number of strands that stabilized in call to run
    uint32_t    _nDead;         //!< number of strands that died in call to run
    worker_cache _strands;
};

/* Worker task for when we do not need super-step synchronization */
static void worker (void *arg)
{
    worker_arg *myArg = reinterpret_cast<worker_arg *>(arg);
    world *wrld = myArg->_wrld;
#ifndef DIDEROT_NO_GLOBALS
    globals *glob = wrld->_globals;
#endif

  // iterate until there is no more work to do
    uint32_t numDead = 0;
    uint32_t numStabilized = 0;
    uint32_t maxSteps = 0;
    uint32_t maxNSteps = myArg->_maxNSteps;
    strand_array::sched_block *blk;
    IF_LOGGING ( LogGetStrandBlock(wrld, myArg->_id+1); )
    while ((blk = myArg->_strands.get_block()) != nullptr) {
        IF_LOGGING ( LogGotStrandBlock(wrld, myArg->_id+1); )
        uint32_t nStable = blk->_nStable;
#ifdef DIDEROT_HAS_STRAND_DIE
        uint32_t nDead = blk->_nDead;
#endif
      // update the strands
        for (auto ix = myArg->_strands.begin_active(blk);
            ix != myArg->_strands.end_active(blk);
        ) {
          // run the strand to completion, or until the step limit is exceeded
            @STRANDTY@ *self = myArg->_strands.strand(ix);
            diderot::strand_status sts = myArg->_strands.status(ix);
#ifdef DIDEROT_HAS_START_METHOD
            if (sts == diderot::kNew) {
                IF_LOGGING ( LogStrandStart(wrld, myArg->_id+1, ix); )
                sts = @STRAND@_start(@START_ARGS@self);
            }
#endif
            uint32_t nSteps = 0;
            while ((! sts) && (nSteps < maxNSteps)) {
                nSteps++;
                sts = @STRAND@_update(@UPDATE_ARGS@self);
            }
            switch (sts) {
              case diderot::kStabilize:
              // stabilize the strand's state.
                IF_LOGGING ( LogStrandStabilize(wrld, myArg->_id+1, ix); )
                ix = myArg->_strands.strand_stabilize (blk, @STABILIZE_ARGS@ix);
                break;
#ifdef DIDEROT_HAS_STRAND_DIE
              case diderot::kDie:
                IF_LOGGING ( LogStrandDie(wrld, myArg->_id+1, ix); )
                ix = myArg->_strands.kill (blk, ix);
                break;
#endif
              default:
                assert (sts == myArg->_strands.status(ix));
                ix = myArg->_strands.next_active(blk, ix);
                break;
            }
            if (maxSteps < nSteps) maxSteps = nSteps;
        }
        numStabilized += (blk->_nStable - nStable);
#ifdef DIDEROT_HAS_STRAND_DIE
        numDead += (blk->_nDead - nDead);
#endif
        IF_LOGGING ( LogGetStrandBlock(wrld, myArg->_id+1); )
    }
    IF_LOGGING ( LogNoStrandBlock(wrld, myArg->_id+1); )

  // update global counts of active and stable strands
    myArg->_nSteps = maxSteps;
    myArg->_nStable = numStabilized;
    myArg->_nDead = numDead;

}
/*---------- end par-worker-nobsp.in ----------*/
  �/*---------- begin par-worker.in ----------*/
struct CACHE_ALIGN worker_arg {
    world       *_wrld;         //!< world pointer
    uint32_t    _id;            //!< worker ID
    uint32_t    _maxNSteps;     //!< maximum number of steps to take; 0 == infinity
    uint32_t    _nSteps;        //!< max number of steps taken by a strand in call to run
    worker_cache _strands;
};

/* Function which processes active strands. */
static void worker (void *arg)
{
    worker_arg *myArg = reinterpret_cast<worker_arg *>(arg);
    world *wrld = myArg->_wrld;
#ifndef DIDEROT_NO_GLOBALS
    globals *glob = wrld->_globals;
#endif
    diderot::scheduler *sched = wrld->_sched;
    worker_cache *strands = &(myArg->_strands);
    bool treeNeedsUpdate = true;

  // barrier before start of first super-step
    sched->_bspBar.all_wait ();

  // iterate until all strands are stable
    uint32_t nSteps = 0;
    uint32_t maxNSteps = myArg->_maxNSteps;

    while ((wrld->_strands.num_active() > 0) && (nSteps < maxNSteps)) {
        uint32_t numDead = 0;
        uint32_t numStabilized = 0;
        strands->refresh();
        nSteps++;
#ifdef DIDEROT_HAS_STRAND_COMMUNICATION
      // build spatial partition to support communication
        if (sched->_bspBar.wait(myArg->_id == 0)) {
          // worker 0 does sequential work of rebuilding tree
/* FIXME: tree building should be parallel */
            IF_LOGGING ( LogKDTreeRebuildStart(wrld, myArg->_id+1); )
            if (treeNeedsUpdate) {
                wrld->_tree->update_strands ();
            }
            wrld->_tree->rebuild ();
            IF_LOGGING ( LogKDTreeRebuildDone(wrld, myArg->_id+1); )
          // synchronize on the tree having been built
            sched->_bspBar.release();
        }
#endif
        strand_array::sched_block *blk;
        while ((blk = strands->get_block()) != nullptr) {
#ifdef DIDEROT_HAS_START_METHOD
          // run start methods for fresh strands
            strands->run_start_methods(@START_ARGS@blk);
#endif
            uint32_t nStable = blk->_nStable;
#ifdef DIDEROT_HAS_STRAND_DIE
            uint32_t nDead = blk->_nDead;
#endif
          // update the strands
            for (auto ix = strands->begin_active(blk);
                ix != strands->end_active(blk);
            ) {
                diderot::strand_status sts = strands->strand_update(@UPDATE_ARGS@ix);
                switch (sts) {
                  case diderot::kStabilize:
                  // stabilize the strand's state.
                    IF_LOGGING ( LogStrandStabilize(wrld, myArg->_id+1, ix); )
                    ix = strands->strand_stabilize(blk, @STABILIZE_ARGS@ix);
                    break;
#ifdef DIDEROT_HAS_STRAND_DIE
                  case diderot::kDie:
                    IF_LOGGING ( LogStrandDie(wrld, myArg->_id+1, ix); )
                    ix = strands->kill (blk, ix);
                    break;
#endif
                  default:
                    assert (sts == strands->status(ix));
                    ix = strands->next_active(blk, ix);
                    break;
                }
            }
          // finish the local-phase of the superstep by updating strand status
            numStabilized += (blk->_nStable - nStable);
#ifdef DIDEROT_HAS_STRAND_DIE
            numDead += (blk->_nDead - nDead);
#endif
        }

        strands->_nStabilizing = numStabilized;
#ifdef DIDEROT_HAS_STRAND_DIE
        strands->_nDying = numDead;
#endif
      // barrier at end of local update phase
        if (sched->_bspBar.wait (myArg->_id == 0)) {
          // finish the local-phase of the superstep by updating strand status
            treeNeedsUpdate = wrld->_strands.finish_step();
            wrld->swap_state();
#ifdef DIDEROT_HAS_GLOBAL_UPDATE
/* FIXME: global update should be parallel */
          // worker 0 does sequential work of global update
            wrld->global_update();
#endif
            sched->_bspBar.release();
        }
        strands->swap();
    }

  // return number of steps
    myArg->_nSteps = nSteps;

}
/*---------- end par-worker.in ----------*/
  n0/*---------- begin par-sarr-dual-indirect.in ----------*/
// forward declaration of worker_cache type
struct worker_cache;
// forward declarations of strand methods
#ifdef DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_start (@START_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#endif // DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#ifdef DIDEROT_HAS_STABILIZE_METHOD
static void @STRAND@_stabilize (@STABILIZE_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#endif // DIDEROT_HAS_STABILIZE_METHOD

// strand_array for PARALLEL_TARGET/BSP/DUAL STATE/INDIRECT ACCESS
//
struct strand_array {
    typedef @STRANDTY@ strand_t;
    typedef uint32_t index_t;           // strand index (index into _idx and _status arrays)
    typedef strand_t *sid_t;            // strand ID (pointer to strand-state storage)
    typedef char *block_t;              // points to array of @STRANDTY@ structs

    // scheduling block of strands
    //
    struct CACHE_ALIGN sched_block {
        index_t         _start;         // first index in block
        index_t         _stop;          // last index in block + 1
        uint32_t        _nStable;       // number of stable strands in the block
        uint32_t        _nDead;         // number of dead strands in the block; this will
                                        // be equal to the block size for unused blocks
      // we organize the strands in a sched_block so that the stable strands are at
      // the beginning, followed by the active strands, followed by the dead strands.
      // An unused block will have _nDead == num_strands()

      // return the number of strands in the block
        uint32_t num_strands () const { return this->_stop - this->_start; }
      // return the number of alive strands in the block
        uint32_t num_alive () const
        {
#ifdef DIDEROT_HAS_STRAND_DIE
            return this->num_strands() - this->_nDead;
#else
            return this->num_strands();
#endif
        }
      // return the number of active strands in the block
        uint32_t num_active () const
        {
            return this->num_alive() - this->_nStable;
        }
      // return index of next available slot in block (_stop if none)
        index_t next_avail () const { return this->_stop - this->_nDead; }

      // is the block being used?
        bool in_use () const { return this->_nDead == this->num_strands(); }
    };

    uint8_t             *_status;       // the array of status information for the strands
    sid_t               *_idx;          // array of strand indices for indirect state rep.
    std::vector<block_t> _blocks;       // vector of pointers to strand-storage blocks
    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling
    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)
    uint32_t            _arraySz;       // the number of allocated items in _status and _idx arrays
    uint32_t            _nStrands;      // number of strands in the _status and _idx arrays
                                        // (including dead strands)
    uint32_t            _nSchedBlks;    // number of scheduling blocks in use
    uint32_t            _nSchedBlksAlloc; // number of allocated scheduling blocks
                                        // INV: _arraySz == _nSchedBlksAlloc * _schedBlkSz.
    uint32_t            _schedBlkSz;    // size of scheduling blocks
    atomic_uint32_t     _nextSchedBlk CACHE_ALIGN;
                                        // next block to schedule
    uint32_t            _nActive;       // global number of active strands
    uint32_t            _nStable;       // global number of stable strands
    pthread_mutex_t     _lock;          // lock for managing access to _blocks vector
    std::vector<worker_cache *> _workers;

  // size info for block_t objects
    static const uint32_t LOG_BLKSZ = 12;               // 2^12 items per block
    static const uint32_t BLKSZ = (1 << LOG_BLKSZ);
    static const uint32_t BLKMASK = (BLKSZ-1);          // mask for block index

    strand_array ()
      : _status(nullptr), _idx(nullptr), _blocks(), _schedBlks(nullptr), _inIdx(0),
        _arraySz(0), _nStrands(0), _nSchedBlks(0), _nSchedBlksAlloc(0), _schedBlkSz(0),
        _nActive(0), _nStable(0), _nextSchedBlk(0),
        _workers()
    {
        pthread_mutex_init (&this->_lock, nullptr);
    }
    ~strand_array ();

    uint32_t in_state_index () const { return this->_inIdx; }

    uint32_t num_active () const { return this->_nActive; }
    uint32_t num_stable () const { return this->_nStable; }
    uint32_t num_alive () const { return this->_nActive+this->_nStable; }

  // return the ID of a strand, which is the value of the _idx array
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nStrands);
        return this->_idx[ix];
    }
  // direct indexing of strands by ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        return id;
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        assert (ix < this->_nStrands);
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }

  // return a pointer to the local state of strand ix
    @STRAND@_local *local_state (index_t ix) const
    {
        return &(this->strand(ix)->_local);
    }
  // return a pointer to the local state of strand with the given ID
    @STRAND@_local *id_to_local_state (sid_t id) const
    {
        return &(this->id_to_strand(id)->_local);
    }

  // return a pointer to the in-state of strand ix
    const @STRAND@_shared *in_state (index_t ix) const
    {
        return &(this->strand(ix)->_shared[this->_inIdx]);
    }
  // return a pointer to the in-state of the strand with the given ID
    const @STRAND@_shared *id_to_in_state (sid_t id) const
    {
        return &(this->id_to_strand(id)->_shared[this->_inIdx]);
    }

  // return a pointer to the out-state of strand ix
    @STRAND@_shared *out_state (index_t ix) const
    {
        return &(this->strand(ix)->_shared[this->_inIdx ^ 1]);
    }

  // deallocate space reserved for strands
    void dealloc ();

  // set the scheduling block size based on the number of workers and the number of
  // strands.  This should be called before alloc.
    void set_block_size (uint32_t nWorkers, uint32_t nStrands)
    {
        this->_schedBlkSz = diderot::sched_block_size (nWorkers, nStrands);
    }

  // allocate space for nItems organized into blkSz sized blocks of strands
    bool alloc (uint32_t nItems);

  // allocated a fresh block of storage for strand states
    block_t *alloc_block ();

  // initialize the first nStrands locations as new active strands
    void create_strands (uint32_t nStrands);

  // swap in and out states
    void swap ()
    {
        this->_inIdx ^= 1;
    }

  // invoke strand's stabilize method (single-thread version)
  // NOTE: because this function does not preserve the sched_block
  // layout invariants, it should only be used for stabilize_all.
    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];
        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];
#ifdef DIDEROT_HAS_STABILIZE_METHOD
      // note that we swap out and in here because out holds the current state
        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);
        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));
#else
        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));
#endif // DIDEROT_HAS_STABILIZE_METHOD
        this->_status[ix] = diderot::kStable;
        this->_nActive--;
        this->_nStable++;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));
        return ix;
    }

  // mark the given strand as dead (single-thread version)
    index_t kill (index_t ix)
    {
        this->_status[ix] = diderot::kDead;
        this->_nActive--;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));
        return ix;
    }

  // prepare to run the workers
    void prepare_run ()
    {
        this->_nextSchedBlk = 0;
    }

  // finish the local-phase of a superstep
    bool finish_step ();

#ifdef DIDEROT_HAS_KILL_ALL // need kill for when step limit expires
  // finish a kill_all operation (NOP)
    void finish_kill_all () { }
#endif

  // finish a stabilize_all operation (NOP)
    void finish_stabilize_all () { }

  // iterator over stable strands
    index_t begin_stable () const
    {
        index_t ix = 0;
        while ((ix < this->_nStrands) && (this->status(ix) != diderot::kStable)) {
            ix++;
        }
        return ix;
    }
    index_t end_stable () const { return this->_nStrands; }
    index_t next_stable (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nStrands) && (this->status(ix) != diderot::kStable));
        return ix;
    }

  // iterator over active strands
    index_t begin_active () const
    {
        index_t ix = 0;
        while ((ix < this->_nStrands) && notActiveSts(this->status(ix))) {
            ix++;
        }
        return ix;
    }
    index_t end_active () const { return this->_nStrands; }
    index_t next_active (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over alive (active+stable) strands
    index_t begin_alive () const
    {
        index_t ix = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nStrands) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }
    index_t end_alive () const { return this->_nStrands; }
    index_t next_alive (index_t &ix) const
    {
        ix++;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nStrands) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }

  // grow the _idx, _status, and _schedBlks arrays
    bool grow (uint32_t n);

}; // struct strand_array

// allocate space for nItems organized into blkSz sized blocks of strands
bool strand_array::alloc (uint32_t nItems)
{
    if (this->_schedBlkSz == 0) {
        std::cerr << "Internal error: strand_array block size is 0\n";
        return true;
    }

// FIXME: if the strands have sequences in them, then we need to invoke "new"!

  // round number of items up to size of storage block
    uint32_t arraySz = (nItems + BLKSZ - 1) & ~BLKMASK;
    uint32_t nBlks = arraySz >> LOG_BLKSZ;
    assert (arraySz == nBlks*BLKSZ);
  // allocate block vector
    this->_blocks.resize(nBlks, nullptr);
  // allocate blocks of storage for strands
    for (int i = 0;  i < nBlks;  i++) {
        this->_blocks[i] = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));
        if (this->_blocks[i] == nullptr) {
          // unable to allocate memory
            this->dealloc();
            return true;
        }
    }

  // allocate _idx, _status, and _schedBlks arrays
    if (this->grow (arraySz)) {
      // unable to allocate memory
        this->dealloc();
        return true;
    }

  // initialize arrays
    index_t ix = 0;
    for (int i = 0;  i < nBlks;  i++) {
        strand_t *p = reinterpret_cast<strand_t *>(this->_blocks[i]);
        for (int j = 0;  j < BLKSZ;  j++, ix++) {
            this->_status[ix] = diderot::kDead;
            this->_idx[ix] = p++;
        }
    }

    this->_arraySz = arraySz;
    this->_nStrands = nItems;
    this->_nActive = 0;
    this->_nStable = 0;

    return false;
}

strand_array::~strand_array ()
{
    pthread_mutex_destroy (&this->_lock);
  // run destructors to reclaim any dynamic memory attached to the strand state
    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {
        this->strand(ix)->~@STRANDTY@();
    }
    this->dealloc();
}

void strand_array::dealloc ()
{
    if (this->_status != nullptr) {
        std::free (this->_status);
        this->_status = nullptr;
    }
    if (this->_idx != nullptr) {
        std::free (this->_idx);
        this->_idx = nullptr;
    }
    if (this->_schedBlks != nullptr) {
        std::free (this->_schedBlks);
        this->_schedBlks = nullptr;
    }
    for (uint32_t i = 0;  i < this->_blocks.size();  i++) {
        if (this->_blocks[i] != nullptr) {
            std::free (this->_blocks[i]);
            this->_blocks[i] = nullptr;
        }
        else {
            break;
        }
    }
}

// initialize the first nStrands locations as new active strands
void strand_array::create_strands (uint32_t nStrands)
{
    assert (this->_nActive == 0);
    assert (this->_arraySz >= nStrands);
    assert (this->_nStrands == nStrands);
    for (index_t ix = 0;  ix < nStrands;  ix++) {
#ifdef DIDEROT_HAS_START_METHOD
        this->_status[ix] = diderot::kNew;
#else
        this->_status[ix] = diderot::kActive;
#endif
        new (this->strand(ix)) @STRANDTY@;
    }
    this->_nActive = nStrands;
//    this->_nFresh = nStrands;
  // initialize the scheduling blocks
    uint32_t lastBlk = nStrands / this->_schedBlkSz;  // index of last in-use block
    index_t ix = 0;
    for (uint32_t i = 0;  i <= lastBlk;  i++) {
        this->_schedBlks[i]._start = ix;
        ix += this->_schedBlkSz;
        this->_schedBlks[i]._stop = ix;
        this->_schedBlks[i]._nDead = 0;
        this->_schedBlks[i]._nStable = 0;
    }
  // adjust the number of dead strands in the last block to account for unused stands
    this->_schedBlks[lastBlk]._nDead = this->_schedBlks[lastBlk]._stop - nStrands;
    this->_nSchedBlks = lastBlk+1;

}

// grow the _idx, _status, and _schedBlks arrays to accomodate at least n additional
// strands
// Note that we do not need to allocate storage space for strands,
// since that is handled by the workers
bool strand_array::grow (uint32_t n)
{
  // round size of arrays to multiple of scheduler block size
    size_t arraySz = static_cast<size_t>(this->_arraySz) + n + this->_schedBlkSz - 1;
    arraySz &= ~(this->_schedBlkSz - 1);

    if (arraySz >= UINT32_MAX) {
      // cannot have more than UINT32_MAX elements
        return true;
    }

  // allocate enough scheduler blocks to cover all of the allocated status/idx items
    uint32_t nSchedBlks = arraySz / this->_schedBlkSz;

  // grow the arrays
    uint8_t *status = static_cast<uint8_t *>(std::malloc (arraySz * sizeof(uint8_t)));
    sid_t *idx = static_cast<sid_t *>(std::malloc (arraySz * sizeof(sid_t)));
    sched_block *schedBlks = static_cast<sched_block *>(std::malloc(nSchedBlks * sizeof(sched_block)));
    if ((status == nullptr) || (idx == nullptr) || (schedBlks == nullptr)) {
        return true;
    }
    if (this->_arraySz > 0) {
        std::memcpy (status, this->_status, this->_arraySz * sizeof(uint8_t));
        std::memcpy (idx, this->_idx, this->_arraySz * sizeof(sid_t));
        std::memcpy (schedBlks, this->_schedBlks, this->_nSchedBlksAlloc * sizeof(sched_block));
      // free the old storage
        std::free (this->_status);
        std::free (this->_idx);
        std::free (this->_schedBlks);
    }

  // initialize new sched_blocks
    uint32_t blkIx = this->_nSchedBlksAlloc;
    index_t ix = blkIx * this->_schedBlkSz;
    for (; blkIx < nSchedBlks;  blkIx++) {
        schedBlks[blkIx]._start = ix;
        ix += this->_schedBlkSz;
        schedBlks[blkIx]._stop = ix;
        schedBlks[blkIx]._nStable = 0;
        schedBlks[blkIx]._nDead = this->_schedBlkSz;
    }

  // update pointers etc.
    this->_status = status;
    this->_idx = idx;
    this->_schedBlks = schedBlks;
    this->_arraySz = arraySz;
    this->_nSchedBlksAlloc = nSchedBlks;

    return false;
}

// a local copy of strand state for workers
struct worker_cache {
    typedef strand_array::strand_t strand_t;
    typedef strand_array::index_t index_t;
    typedef strand_array::sid_t sid_t;
    typedef strand_array::block_t block_t;
    typedef strand_array::sched_block sched_block;

    strand_array        *_sarray;       // pointer to global strand_array structure
    uint8_t             *_status;       // the array of status information for the strands
    sid_t               *_idx;          // array of strand indices for indirect state rep.
    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling
    atomic_uint32_t     *_nextBlkPtr;   // pointer to _nextSchedBlk
    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)
    uint32_t            _nStabilizing;  // count of strands run by this worker that stabilized in
                                        // the current superstep
#ifdef DIDEROT_HAS_STRAND_DIE
    uint32_t            _nDying;        // count of strands run by this worker that died in
                                        // the current superstep
#endif
    uint32_t            _nSchedBlks;    // number of scheduling blocks
    uint32_t            _schedBlkSz;    // size of scheduling blocks
#ifndef NDEBUG
    uint32_t            _nStrands;      // number of strands in the _idx and _status arrays
#endif
    block_t             _newBlock;      // strand-storage block for new strands
    strand_t            *_nextStrand;   // allocation pointer for new strands; should point inside
                                        // the _newBlock
    strand_t            *_limitPtr;     // limit pointer for new-strand allocation
    std::vector<sid_t>  _fresh;         // fresh strands created in current superstep

  // allocate a block of storage for new strands; returns true if there is an error
    bool alloc_block ();

    void init (strand_array &sarr)
    {
        this->_sarray = &sarr;
        this->_status = sarr._status;
        this->_idx = sarr._idx;
        this->_schedBlks = sarr._schedBlks;
        this->_nextBlkPtr = &sarr._nextSchedBlk;
        this->_inIdx = sarr._inIdx;
        this->_nStabilizing = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
        this->_nDying = 0;
#endif
        this->_nSchedBlks = sarr._nSchedBlks;
        this->_schedBlkSz = sarr._schedBlkSz;
#ifndef NDEBUG
        this->_nStrands = sarr._nStrands;
#endif
        this->_nextStrand = nullptr;
        this->_limitPtr = nullptr;
        sarr._workers.push_back (this);
    }

  // refresh those parts of the cache that might change between steps
    void refresh ()
    {
        this->_status = this->_sarray->_status;
        this->_idx = this->_sarray->_idx;
        this->_nStabilizing = 0; /* QUESTION: is this the correct place for this? */
#ifdef DIDEROT_HAS_STRAND_DIE
        this->_nDying = 0;
#endif
        this->_schedBlks = this->_sarray->_schedBlks;
        this->_nSchedBlks = this->_sarray->_nSchedBlks;
#ifndef NDEBUG
        this->_nStrands = this->_sarray->_nStrands;
#endif
    }

  // return the ID of a strand, which is the value of the _idx array
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nStrands);
        return this->_idx[ix];
    }
  // direct indexing of strands by ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        return id;
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        assert (ix < this->_nStrands);
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }

#ifdef DIDEROT_HAS_START_METHOD
  // invoke strand's start method
    diderot::strand_status strand_start (@START_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        return @STRAND@_start (@START_ARGS@
            &self->_local,
            &self->_shared[this->_inIdx],
            &self->_shared[this->_inIdx^1]);
    }

    void run_start_methods (@START_PARAMS@sched_block *bp);
#endif // DIDEROT_HAS_START_METHOD

  // invoke strand's update method
    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        return @STRAND@_update (@UPDATE_ARGS@
            &self->_local,
            &self->_shared[this->_inIdx],
            &self->_shared[this->_inIdx^1]);
    }

  // invoke strand's stabilize method (multithread version)
    index_t strand_stabilize (sched_block *bp, @STABILIZE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];
        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];
#ifdef DIDEROT_HAS_STABILIZE_METHOD
      // note that we swap out and in here because out holds the current state
        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);
        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));
#else
        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));
#endif // DIDEROT_HAS_STABILIZE_METHOD
      // we swap the strand-indices at ix and bp->_start + bp->_nStable
        uint32_t jx = bp->_start + bp->_nStable;
        this->_status[jx] = diderot::kStable;
        std::swap (this->_idx[ix], this->_idx[jx]);
        bp->_nStable++;
        return ix+1;
    }

  // mark the given strand as dead (multithread version)
    index_t kill (sched_block *bp, index_t ix)
    {
        assert (bp->_start + bp->_nStable <= ix);
        assert (ix < bp->_start + bp->num_alive());
        bp->_nDead++;
      // swap the strand at ix with the last active strand in the block
        uint32_t jx = bp->_stop - bp->_nDead;
        this->_status[jx] = diderot::kDead;
        std::swap (this->_idx[ix], this->_idx[jx]);
        return ix;  // don't advance, since ix is an active strand after the swap
    }

  // wrappers for accessing the state of newly created strands
    @STRAND@_local *new_local_state (index_t ix) const
    {
        return &(this->id_to_strand(this->_fresh[ix])->_local);
    }
    @STRAND@_shared *new_out_state (index_t ix) const
    {
        return &(this->id_to_strand(this->_fresh[ix])->_shared[this->_inIdx ^ 1]);
    }

    index_t new_strand ()
    {
        index_t ix = this->_fresh.size();
        if (this->_nextStrand >= this->_limitPtr) {
            if (this->alloc_block()) {
                std::cerr << "Fatal error: unable to allocate space for new strands" << std::endl;
                exit (1);
            }
        }
        strand_t *strand = this->_nextStrand;
        this->_nextStrand++;
        this->_fresh.push_back (strand);
        new (strand) @STRANDTY@;
        return ix;
    }

  // iterator over active strands in a scheduling block
    index_t begin_active (const sched_block *bp) const { return bp->_start + bp->_nStable; }
    index_t end_active (const sched_block *bp) const { return bp->_stop - bp->_nDead; }
    index_t next_active (const sched_block *bp, index_t &ix) const { return ++ix; }

  // iterator over fresh strands in a scheduling block
    index_t begin_fresh (const sched_block *bp) const
    {
        index_t ix = this->begin_active(bp);
        while ((ix != this->end_active(bp)) && (this->status(ix) != diderot::kNew)) {
            ix = this->next_active(bp, ix);
        }
        return ix;
    }
    index_t end_fresh (const sched_block *bp) const { return this->end_active(bp); }
    index_t next_fresh (const sched_block *bp, index_t &ix) const
    {
        do {
            ix = this->next_active(bp, ix);
        } while ((ix != this->end_active(bp)) && (this->status(ix) != diderot::kNew));
        return ix;
    }

  // swap in and out states
    void swap ()
    {
        this->_inIdx ^= 1;
    }

  // get a block of strands
    sched_block *get_block ();

}; // struct worker_cache

strand_array::sched_block *worker_cache::get_block ()
{
    do {
        uint32_t blkId = this->_nextBlkPtr->fetch_add(1);
        if (blkId < this->_nSchedBlks) {
            strand_array::sched_block *bp = &this->_schedBlks[blkId];
            if (bp->num_active() > 0) {
                return bp;
            } // else skip stable block
        }
        else {  // no more blocks
            return nullptr;
        }
    } while (true);

}

bool worker_cache::alloc_block ()
{
    pthread_mutex_lock(&this->_sarray->_lock);
        char *blk = static_cast<block_t>(std::malloc (strand_array::BLKSZ * sizeof(@STRANDTY@)));
        if (blk == nullptr) {
            pthread_mutex_unlock(&this->_sarray->_lock);
            return true;
        }
        this->_sarray->_blocks.push_back(blk);
    pthread_mutex_unlock(&this->_sarray->_lock);

    this->_newBlock = blk;
    this->_nextStrand = reinterpret_cast<strand_t *>(blk);
    this->_limitPtr = reinterpret_cast<strand_t *>(blk + strand_array::BLKSZ * sizeof(@STRANDTY@));

    return false;
}

// finish the update phase of a superstep by compacting
// strands and including any fresh strands from the other
// workers.  Return true if there are any new or dead strands.
bool strand_array::finish_step ()
{
    int32_t nStabilizing = 0;
    int32_t nNew = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
    int32_t nDying = 0;
#endif

    int32_t blkIx = 0;
    for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {
        worker_cache *wp = *it;
        nStabilizing += wp->_nStabilizing;
        nNew += wp->_fresh.size();
#ifdef DIDEROT_HAS_STRAND_DIE
        nDying += wp->_nDying;
#endif
    }

    if (nNew > 0) {
      // the number of currently unused scheduler-block slots
        index_t nAvail = this->_schedBlkSz * this->_nSchedBlksAlloc
            - this->_nActive - this->_nStable;
        if (nAvail < nNew) {
          // we need to grow the _status, _idx, and _schedBlk arrays
            this->grow (nNew - nAvail);
        }
        assert (this->_arraySz == this->_nSchedBlksAlloc * this->_schedBlkSz);
#ifdef DIDEROT_HAS_STRAND_DIE
        this->_nStrands += nNew - nDying;
#else
        this->_nStrands += nNew;
#endif
      // copy fresh strands into the unused slots
        sched_block *bp = this->_schedBlks;
        index_t nextIx = bp->next_avail();
        uint32_t nBlks = 1;
        for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {
            worker_cache *wp = *it;
            for (auto jt = wp->_fresh.begin();  jt != wp->_fresh.end();  ++jt) {
              // advance to the next free slot
                while (nextIx == bp->_stop) {
                    bp++;
                    nBlks++;
                    nextIx = bp->next_avail();
                }
                assert (bp < this->_schedBlks + this->_nSchedBlksAlloc);
                this->_idx[nextIx] = *jt;
#ifdef DIDEROT_HAS_START_METHOD
                this->_status[nextIx] = diderot::kNew;
#else
                this->_status[nextIx] = diderot::kActive;
#endif
                nextIx++;
                bp->_nDead--;
            }
            wp->_fresh.clear();
        }
        this->_nSchedBlks = nBlks;
    }
#ifdef DIDEROT_HAS_STRAND_DIE
    else if (nDying > 0) {
      /* FIXME: compact dead strands */
/*
      // check to see if we need to compact dead strands?
        if ((this->_nStrands - this->_nActive) / this->_schedBlkSz > ??) {
        }
*/
    }
#endif

  // reset scheduler for next superstep
    this->_nextSchedBlk = 0;

  // update global count of stable strands
    this->_nStable += nStabilizing;
  // update global count of active strands
#ifdef DIDEROT_HAS_STRAND_DIE
    this->_nActive += nNew - (nStabilizing + nDying);

    return (nNew + nDying) > 0;
#else
    this->_nActive += nNew - nStabilizing;

    return nNew > 0;
#endif

}
/*---------- end par-sarr-dual-indirect.in ----------*/
  I�/*---------- begin par-sarr-dual.in ----------*/
// forward declaration of worker_cache type
struct worker_cache;
// forward declarations of strand methods
#ifdef DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_start (@START_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#endif // DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#ifdef DIDEROT_HAS_STABILIZE_METHOD
static void @STRAND@_stabilize (@STABILIZE_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#endif // DIDEROT_HAS_STABILIZE_METHOD

#ifdef DIDEROT_HAS_STRAND_DIE
#  error unexpected presence of "die"
#endif

// strand_array for PARALLEL_TARGET/BSP/DUAL STATE/DIRECT ACCESS
//
struct strand_array {
    typedef @STRANDTY@ strand_t;
    typedef uint32_t index_t;
    typedef index_t sid_t;              // strand ID (index into strand-state storage)

    // scheduling block of strands
    //
    struct CACHE_ALIGN sched_block {
        index_t         _start;         // first index in block
        index_t         _stop;          // last index in block + 1
        uint32_t        _nStable;       // number of stable strands in the block

      // return the number of strands in the block
        uint32_t num_strands () const { return this->_stop - this->_start; }
      // return the number of active strands in the block
        uint32_t num_active () const
        {
            return this->num_strands() - this->_nStable;
        }
    };

    uint8_t             *_status;       // the array of status information for the strands
    char                *_storage;      // points to array of @STRANDTY@ structs
    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)
    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling
    uint32_t            _nItems;        // number of items in the _storage and _status arrays
    uint32_t            _nFresh;        // number of fresh strands (new strands from create_strands)
    uint32_t            _nBlks;         // number of scheduling blocks
    uint32_t            _blkSz;         // size of scheduling blocks
    atomic_uint32_t     _nStable CACHE_ALIGN;
                                        // global number of stable strands
    atomic_uint32_t     _nActive CACHE_ALIGN;
                                        // global number of active strands
    atomic_uint32_t     _nextSchedBlk CACHE_ALIGN;
                                        // next block to schedule
    std::vector<worker_cache *> _workers;

    strand_array ()
        : _status(nullptr), _storage(nullptr), _schedBlks(nullptr), _nItems(0),
          _nStable(0), _nActive(0), _nFresh(0), _nBlks(0), _blkSz(0), _nextSchedBlk(0)
    { }
    ~strand_array ();

    uint32_t in_state_index () const { return this->_inIdx; }

    uint32_t num_active () const { return this->_nActive; }
    uint32_t num_stable () const { return this->_nStable; }
    uint32_t num_alive () const { return this->_nActive+this->_nStable; }

  // return the ID of a strand, which is the value of the _idx array
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nItems);
        return ix;
    }
  // return a pointer to the strand with the given ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        assert (id < this->_nItems);
        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }
  // return a pointer to the local state of strand ix
    @STRAND@_local *local_state (index_t ix) const
    {
        return &(this->strand(ix)->_local);
    }
  // return a pointer to the local state of strand with the given ID
    @STRAND@_local *id_to_local_state (sid_t id) const
    {
        return &(this->id_to_strand(id)->_local);
    }
  // return a pointer to the in-state of strand ix
    const @STRAND@_shared *in_state (index_t ix) const
    {
        return &(this->strand(ix)->_shared[this->_inIdx]);
    }
  // return a pointer to the in-state of the strand with the given ID
    const @STRAND@_shared *id_to_in_state (sid_t id) const
    {
        return &(this->id_to_strand(id)->_shared[this->_inIdx]);
    }
  // return a pointer to the out-state of strand ix
    @STRAND@_shared *out_state (index_t ix) const
    {
        return &(this->strand(ix)->_shared[this->_inIdx ^ 1]);
    }

  // set the scheduling block size based on the number of workers and the number of
  // strands.  This should be called before alloc.
    void set_block_size (uint32_t nWorkers, uint32_t nStrands)
    {
        this->_blkSz = diderot::sched_block_size (nWorkers, nStrands);
    }

  // allocate space for nItems organized into blkSz sized blocks of strands
    bool alloc (uint32_t nItems);

  // initialize the first nStrands locations as new active strands
    void create_strands (uint32_t nStrands);

  // swap in and out states
    void swap ()
    {
        this->_inIdx ^= 1;
// FIXME: once we have parallel reductions and parallel tree building, we will need
// to reset this counter in other places too
        this->_nextSchedBlk = 0;
    }

  // invoke strand's stabilize method (single-thread version)
    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];
        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];
#ifdef DIDEROT_HAS_STABILIZE_METHOD
      // note that we swap out and in here because out holds the current state
        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);
        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));
#else
        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));
#endif // DIDEROT_HAS_STABILIZE_METHOD
        this->_status[ix] = diderot::kStable;
        this->_nActive--;
        this->_nStable++;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

#ifdef DIDEROT_HAS_KILL_ALL // need kill for when step limit expires
  // mark the given strand as dead (single-thread version)
    index_t kill (index_t ix)
    {
        this->_status[ix] = diderot::kDead;
        this->_nActive--;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }
#endif

  // prepare to run the workers
    void prepare_run ()
    {
        this->_nextSchedBlk = 0;
    }

  // finish the local-phase of a superstep
    bool finish_step ();

#ifdef DIDEROT_HAS_KILL_ALL // need kill for when step limit expires
  // finish a kill_all operation (NOP)
    void finish_kill_all () { }
#endif

  // finish a stabilize_all operation (NOP)
    void finish_stabilize_all () { }

  // iterator over all alive strands (single-threaded version)
    index_t begin_alive () const
    {
        index_t ix = 0;
        return ix;
    }
    index_t end_alive () const { return this->_nItems; }
    index_t next_alive (index_t &ix) const
    {
        ix++;
        return ix;
    }

  // iterator over all active strands (single-threaded version)
    index_t begin_active () const
    {
        index_t ix = 0;
        while ((ix < this->_nItems) && notActiveSts(this->status(ix))) {
            ix++;
        }
        return ix;
    }
    index_t end_active () const { return this->_nItems; }
    index_t next_active (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over stable strands
    index_t begin_stable () const
    {
        index_t ix = 0;
        while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable)) {
            ix++;
        }
        return ix;
    }
    index_t end_stable () const { return this->_nItems; }
    index_t next_stable (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable));
        return ix;
    }

  // iterator over fresh strands; since the only new strands were created by create_strand
  // we iterate over all of them
    index_t begin_fresh () const { return 0; }
    index_t end_fresh () const { return this->_nFresh; }
    index_t next_fresh (index_t &ix) const { return ++ix; }

}; // struct strand_array

strand_array::~strand_array ()
{
  // run destructors to reclaim any dynamic memory attached to the strand state
    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {
        this->strand(ix)->~@STRANDTY@();
    }
    if (this->_status != nullptr) std::free (this->_status);
    if (this->_storage != nullptr) std::free (this->_storage);
    if (this->_schedBlks != nullptr) std::free (this->_schedBlks);
}

bool strand_array::alloc (uint32_t nItems)
{
    if (this->_blkSz == 0) {
        std::cerr << "Internal error: strand_array block size is 0\n";
        return true;
    }
    this->_storage = static_cast<char *>(std::malloc (nItems * sizeof(@STRANDTY@)));
    if (this->_storage == nullptr) {
        return true;
    }
    this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));
    if (this->_status == nullptr) {
        std::free (this->_storage);
        return true;
    }
    this->_nBlks = (nItems + this->_blkSz - 1) / this->_blkSz;
    this->_schedBlks = static_cast<sched_block *>(std::malloc (this->_nBlks * sizeof(sched_block)));
    if (this->_schedBlks == nullptr) {
        std::free (this->_storage);
        std::free (this->_status);
        return true;
    }
    this->_inIdx = 0;
    this->_nItems = nItems;
    this->_nActive = 0;
    this->_nStable = 0;
    this->_nFresh = 0;
    return false;
}

void strand_array::create_strands (uint32_t nStrands)
{
    assert (this->_nActive == 0);
    assert (this->_nItems == nStrands);
    for (uint32_t ix = 0;  ix < nStrands;  ix++) {
#ifdef DIDEROT_HAS_START_METHOD
        this->_status[ix] = diderot::kNew;
#else
        this->_status[ix] = diderot::kActive;
#endif
        new(this->strand(ix)) @STRANDTY@;
    }
    this->_nActive = nStrands;
    this->_nFresh = nStrands;
  // initialize the scheduling blocks
    for (uint32_t ix = 0, i = 0;  i < this->_nBlks;  i++) {
        this->_schedBlks[i]._start = ix;
        ix += this->_blkSz;
        this->_schedBlks[i]._stop = ix;
        this->_schedBlks[i]._nStable = 0;
    }
  // the last block may be incomplete, so adjust it
    this->_schedBlks[this->_nBlks-1]._stop = nStrands;
}

// a local copy of strand state for workers
struct worker_cache {
    typedef strand_array::strand_t strand_t;
    typedef strand_array::index_t index_t;
    typedef strand_array::sid_t sid_t;
    typedef strand_array::sched_block sched_block;

    uint8_t             *_status;       // the array of status information for the strands
    char                *_storage;      // points to array of @STRANDTY@ structs
    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling
    atomic_uint32_t     *_nStablePtr;   // pointer to _nStable
    atomic_uint32_t     *_nActivePtr;   // pointer to _nActive
    atomic_uint32_t     *_nextBlkPtr;   // pointer to _nextSchedBlk
    uint32_t            _nStabilizing;  // count of strands run by this worker that stabilized in
                                        // the current superstep
#ifdef DIDEROT_HAS_STRAND_DIE
    uint32_t            _nDying;        // count of strands run by this worker that died in
                                        // the current superstep
#endif
    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)
    uint32_t            _nBlks;         // number of scheduling blocks
    uint32_t            _blkSz;         // size of scheduling blocks
#ifndef NDEBUG
    uint32_t        _nItems;        // number of items in the _storage and _status arrays
#endif

    void init (strand_array &sarr)
    {
        this->_status = sarr._status;
        this->_storage = sarr._storage;
        this->_schedBlks = sarr._schedBlks;
        this->_nStablePtr = &sarr._nStable;
        this->_nActivePtr = &sarr._nActive;
        this->_nextBlkPtr = &sarr._nextSchedBlk;
        this->_nStabilizing = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
        this->_nDying = 0;
#endif
        this->_inIdx = sarr._inIdx;
        this->_nBlks = sarr._nBlks;
        this->_blkSz = sarr._blkSz;
#ifndef NDEBUG
        this->_nItems = sarr._nItems;
#endif
        sarr._workers.push_back (this);
    }

  // refresh those parts of the cache that might change between steps
    void refresh ()
    {
        // this target does not support dynamic strands, so nothing can change
    }

  // return the ID of a strand, which is the value of the _idx array
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nItems);
        return ix;
    }
  // return a pointer to the strand with the given ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));
    }
  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }

  // swap in and out states
    void swap ()
    {
        this->_inIdx ^= 1;
    }

#ifdef DIDEROT_HAS_START_METHOD
  // invoke strand's start method
    diderot::strand_status strand_start (@START_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        return @STRAND@_start (@START_ARGS@
            &self->_local,
            &self->_shared[this->_inIdx],
            &self->_shared[this->_inIdx^1]);
    }

    void run_start_methods (@START_PARAMS@sched_block *bp);
#endif // DIDEROT_HAS_START_METHOD

  // invoke strand's update method
    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        return @STRAND@_update (@UPDATE_ARGS@
            &self->_local,
            &self->_shared[this->_inIdx],
            &self->_shared[this->_inIdx^1]);
    }

  // invoke strand's stabilize method (multithread version)
    index_t strand_stabilize (sched_block *bp, @STABILIZE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];
        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];
#ifdef DIDEROT_HAS_STABILIZE_METHOD
      // note that we swap out and in here because out holds the current state
        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);
        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));
#else
        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));
#endif // DIDEROT_HAS_STABILIZE_METHOD
        this->_status[ix] = diderot::kStable;
        bp->_nStable++;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over alive strands in a scheduling block
    index_t begin_alive (const sched_block *bp) const
    {
        index_t ix = bp->_start;
        return ix;
    }
    index_t end_alive (const sched_block *bp) const { return bp->_stop; }
    index_t next_alive (const sched_block *bp, index_t &ix) const
    {
        return ix;
    }

  // iterator over active strands in a scheduling block
    index_t begin_active (const sched_block *bp) const
    {
        index_t ix = bp->_start;
        while ((ix < bp->_stop) && notActiveSts(this->status(ix))) {
            ix++;
        }
        return ix;
    }
    index_t end_active (const sched_block *bp) const { return bp->_stop; }
    index_t next_active (const sched_block *bp, index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over fresh strands in a scheduling block
    index_t begin_fresh (const sched_block *bp) const
    {
        index_t ix = bp->_start;
        while ((ix < bp->_stop) && (this->status(ix) != diderot::kNew)) {
            ix++;
        }
        return ix;
    }
    index_t end_fresh (const sched_block *bp) const { return bp->_stop; }
    index_t next_fresh (const sched_block *bp, index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < bp->_stop) && (this->status(ix) != diderot::kNew));
        return ix;
    }

  // get a block of strands
    sched_block *get_block ();

}; // struct worker_cache

strand_array::sched_block *worker_cache::get_block ()
{
    do {
        uint32_t blkId = this->_nextBlkPtr->fetch_add(1);
        if (blkId < this->_nBlks) {
            strand_array::sched_block *bp = &this->_schedBlks[blkId];
            if (bp->num_active() > 0) {
                return bp;
            } // else skip stable block
        }
        else {  // no more blocks
            return nullptr;
        }
    } while (true);

}

// finish the update phase of a superstep.    Return true if there are any dead strands.
bool strand_array::finish_step ()
{
    int32_t nStabilizing = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
    int32_t nDying = 0;
#endif

    for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {
        worker_cache *wp = *it;
        nStabilizing += wp->_nStabilizing;
#ifdef DIDEROT_HAS_STRAND_DIE
        nDying += wp->_nDying;
#endif
    }

#ifdef DIDEROT_HAS_STRAND_DIE
    if (nDying > 0) {
      /* FIXME: compact dead strands */
/*
      // check to see if we need to compact dead strands?
        if ((this->_nStrands - this->_nActive) / this->_schedBlkSz > ??) {
        }
*/
    }
#endif

  // reset scheduler for next superstep
    this->_nextSchedBlk = 0;

  // update global count of stable strands
    this->_nStable += nStabilizing;
  // update global count of active strands
#ifdef DIDEROT_HAS_STRAND_DIE
    this->_nActive -= (nStabilizing + nDying);

    return (nDying > 0);
#else
    this->_nActive -= nStabilizing;

    return false;
#endif

}
/*---------- end par-sarr-dual.in ----------*/
  ft/*---------- begin par-sarr-indirect.in ----------*/
// forward declaration of worker_cache type
struct worker_cache;
// forward declarations of strand methods
#ifdef DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_start (@START_PARAMS@@STRANDTY@ *self);
#endif // DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@@STRANDTY@ *self);
#ifdef DIDEROT_HAS_STABILIZE_METHOD
static void @STRAND@_stabilize (@STABILIZE_PARAMS@@STRANDTY@ *self);
#endif // DIDEROT_HAS_STABILIZE_METHOD

// strand_array for PARALLEL_TARGET/BSP/SINGLE STATE/INDIRECT ACCESS
//
struct strand_array {
    typedef @STRANDTY@ strand_t;
    typedef uint32_t index_t;           // strand index (index into _idx and _status arrays)
    typedef strand_t *sid_t;            // strand ID (pointer to strand-state storage)
    typedef char *block_t;              // points to array of @STRANDTY@ structs

    // scheduling block of strands
    //
    struct CACHE_ALIGN sched_block {
        index_t         _start;         // first index in block
        index_t         _stop;          // last index in block + 1
        uint32_t        _nStable;       // number of stable strands in the block
        uint32_t        _nDead;         // number of dead strands in the block; this will
                                        // be equal to the block size for unused blocks
      // we organize the strands in a sched_block so that the stable strands are at
      // the beginning, followed by the active strands, followed by the dead strands.
      // An unused block will have _nDead == num_strands()

      // return the number of strands in the block
        uint32_t num_strands () const { return this->_stop - this->_start; }
      // return the number of alive strands in the block
        uint32_t num_alive () const
        {
#ifdef DIDEROT_HAS_STRAND_DIE
            return this->num_strands() - this->_nDead;
#else
            return this->num_strands();
#endif
        }
      // return the number of active strands in the block
        uint32_t num_active () const
        {
            return this->num_alive() - this->_nStable;
        }
      // return index of next available slot in block (_stop if none)
        index_t next_avail () const { return this->_stop - this->_nDead; }

      // is the block being used?
        bool in_use () const { return this->_nDead == this->num_strands(); }
    };

    uint8_t             *_status;       // the array of status information for the strands
    sid_t               *_idx;          // array of strand indices for indirect state rep.
    std::vector<block_t> _blocks;       // vector of pointers to strand-storage blocks
    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling
    uint32_t            _arraySz;       // the number of allocated items in _status and _idx arrays
    uint32_t            _nStrands;      // number of strands in the _status and _idx arrays
                                        // (including dead strands)
    uint32_t            _nSchedBlks;    // number of scheduling blocks in use
    uint32_t            _nSchedBlksAlloc; // number of allocated scheduling blocks
                                        // INV: _arraySz == _nSchedBlksAlloc * _schedBlkSz.
    uint32_t            _schedBlkSz;    // size of scheduling blocks
    atomic_uint32_t     _nextSchedBlk CACHE_ALIGN;
                                        // next block to schedule
    uint32_t            _nActive;       // global number of active strands
    uint32_t            _nStable;       // global number of stable strands
    pthread_mutex_t     _lock;          // lock for managing access to _blocks vector
    std::vector<worker_cache *> _workers;

  // size info for block_t objects
    static const uint32_t LOG_BLKSZ = 12;               // 2^12 items per block
    static const uint32_t BLKSZ = (1 << LOG_BLKSZ);
    static const uint32_t BLKMASK = (BLKSZ-1);          // mask for block index

    strand_array ()
      : _status(nullptr), _idx(nullptr), _blocks(), _schedBlks(nullptr),
        _arraySz(0), _nStrands(0), _nSchedBlks(0), _nSchedBlksAlloc(0), _schedBlkSz(0),
        _nActive(0), _nStable(0), _nextSchedBlk(0),
        _workers()
    {
        pthread_mutex_init (&this->_lock, nullptr);
    }
    ~strand_array ();

    uint32_t in_state_index () const { return 0; /* dummy */ }

    uint32_t num_active () const { return this->_nActive; }
    uint32_t num_stable () const { return this->_nStable; }
    uint32_t num_alive () const { return this->_nActive+this->_nStable; }

  // return the ID of a strand, which is the value of the _idx array
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nStrands);
        return this->_idx[ix];
    }
  // direct indexing of strands by ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        return id;
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        assert (ix < this->_nStrands);
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }

  // return a pointer to the local state of strand ix
    @STRANDTY@ *local_state (index_t ix) const
    {
        return this->strand(ix);
    }
  // return a pointer to the local state of strand with the given ID
    @STRANDTY@ *id_to_local_state (sid_t id) const
    {
        return this->id_to_strand(id);
    }

  // deallocate space reserved for strands
    void dealloc ();

  // set the scheduling block size based on the number of workers and the number of
  // strands.  This should be called before alloc.
    void set_block_size (uint32_t nWorkers, uint32_t nStrands)
    {
        this->_schedBlkSz = diderot::sched_block_size (nWorkers, nStrands);
    }

  // allocate space for nItems organized into blkSz sized blocks of strands
    bool alloc (uint32_t nItems);

  // allocated a fresh block of storage for strand states
    block_t *alloc_block ();

  // initialize the first nStrands locations as new active strands
    void create_strands (uint32_t nStrands);

  // swap in and out states (NOP for this version)
    void swap () { }

  // invoke strand's stabilize method (single-thread version)
  // NOTE: because this function does not preserve the sched_block
  // layout invariants, it should only be used for stabilize_all.
    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)
    {
#ifdef DIDEROT_HAS_STABILIZE_METHOD
        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));
#endif // DIDEROT_HAS_STABILIZE_METHOD
        this->_status[ix] = diderot::kStable;
        this->_nActive--;
        this->_nStable++;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));
        return ix;
    }

  // mark the given strand as dead (single-thread version)
    index_t kill (index_t ix)
    {
        this->_status[ix] = diderot::kDead;
        this->_nActive--;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));
        return ix;
    }

  // prepare to run the workers
    void prepare_run ()
    {
        this->_nextSchedBlk = 0;
    }

  // finish the local-phase of a superstep
    bool finish_step ();

#ifdef DIDEROT_HAS_KILL_ALL // need kill for when step limit expires
  // finish a kill_all operation (NOP)
    void finish_kill_all () { }
#endif

  // finish a stabilize_all operation (NOP)
    void finish_stabilize_all () { }

  // iterator over stable strands
    index_t begin_stable () const
    {
        index_t ix = 0;
        while ((ix < this->_nStrands) && (this->status(ix) != diderot::kStable)) {
            ix++;
        }
        return ix;
    }
    index_t end_stable () const { return this->_nStrands; }
    index_t next_stable (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nStrands) && (this->status(ix) != diderot::kStable));
        return ix;
    }

  // iterator over active strands
    index_t begin_active () const
    {
        index_t ix = 0;
        while ((ix < this->_nStrands) && notActiveSts(this->status(ix))) {
            ix++;
        }
        return ix;
    }
    index_t end_active () const { return this->_nStrands; }
    index_t next_active (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over alive (active+stable) strands
    index_t begin_alive () const
    {
        index_t ix = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nStrands) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }
    index_t end_alive () const { return this->_nStrands; }
    index_t next_alive (index_t &ix) const
    {
        ix++;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nStrands) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }

  // grow the _idx, _status, and _schedBlks arrays
    bool grow (uint32_t n);

}; // struct strand_array

// allocate space for nItems organized into blkSz sized blocks of strands
bool strand_array::alloc (uint32_t nItems)
{
    if (this->_schedBlkSz == 0) {
        std::cerr << "Internal error: strand_array block size is 0\n";
        return true;
    }

// FIXME: if the strands have sequences in them, then we need to invoke "new"!

  // round number of items up to size of storage block
    uint32_t arraySz = (nItems + BLKSZ - 1) & ~BLKMASK;
    uint32_t nBlks = arraySz >> LOG_BLKSZ;
    assert (arraySz == nBlks*BLKSZ);
  // allocate block vector
    this->_blocks.resize(nBlks, nullptr);
  // allocate blocks of storage for strands
    for (int i = 0;  i < nBlks;  i++) {
        this->_blocks[i] = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));
        if (this->_blocks[i] == nullptr) {
          // unable to allocate memory
            this->dealloc();
            return true;
        }
    }

  // allocate _idx, _status, and _schedBlks arrays
    if (this->grow (arraySz)) {
      // unable to allocate memory
        this->dealloc();
        return true;
    }

  // initialize arrays
    index_t ix = 0;
    for (int i = 0;  i < nBlks;  i++) {
        strand_t *p = reinterpret_cast<strand_t *>(this->_blocks[i]);
        for (int j = 0;  j < BLKSZ;  j++, ix++) {
            this->_status[ix] = diderot::kDead;
            this->_idx[ix] = p++;
        }
    }

    this->_arraySz = arraySz;
    this->_nStrands = nItems;
    this->_nActive = 0;
    this->_nStable = 0;

    return false;
}

strand_array::~strand_array ()
{
    pthread_mutex_destroy (&this->_lock);
  // run destructors to reclaim any dynamic memory attached to the strand state
    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {
        this->strand(ix)->~@STRANDTY@();
    }
    this->dealloc();
}

void strand_array::dealloc ()
{
    if (this->_status != nullptr) {
        std::free (this->_status);
        this->_status = nullptr;
    }
    if (this->_idx != nullptr) {
        std::free (this->_idx);
        this->_idx = nullptr;
    }
    if (this->_schedBlks != nullptr) {
        std::free (this->_schedBlks);
        this->_schedBlks = nullptr;
    }
    for (uint32_t i = 0;  i < this->_blocks.size();  i++) {
        if (this->_blocks[i] != nullptr) {
            std::free (this->_blocks[i]);
            this->_blocks[i] = nullptr;
        }
        else {
            break;
        }
    }
}

// initialize the first nStrands locations as new active strands
void strand_array::create_strands (uint32_t nStrands)
{
    assert (this->_nActive == 0);
    assert (this->_arraySz >= nStrands);
    assert (this->_nStrands == nStrands);
    for (index_t ix = 0;  ix < nStrands;  ix++) {
#ifdef DIDEROT_HAS_START_METHOD
        this->_status[ix] = diderot::kNew;
#else
        this->_status[ix] = diderot::kActive;
#endif
        new (this->strand(ix)) @STRANDTY@;
    }
    this->_nActive = nStrands;
//    this->_nFresh = nStrands;
  // initialize the scheduling blocks
    uint32_t lastBlk = nStrands / this->_schedBlkSz;  // index of last in-use block
    index_t ix = 0;
    for (uint32_t i = 0;  i <= lastBlk;  i++) {
        this->_schedBlks[i]._start = ix;
        ix += this->_schedBlkSz;
        this->_schedBlks[i]._stop = ix;
        this->_schedBlks[i]._nDead = 0;
        this->_schedBlks[i]._nStable = 0;
    }
  // adjust the number of dead strands in the last block to account for unused stands
    this->_schedBlks[lastBlk]._nDead = this->_schedBlks[lastBlk]._stop - nStrands;
    this->_nSchedBlks = lastBlk+1;

}

// grow the _idx, _status, and _schedBlks arrays to accomodate at least n additional
// strands
// Note that we do not need to allocate storage space for strands,
// since that is handled by the workers
bool strand_array::grow (uint32_t n)
{
  // round size of arrays to multiple of scheduler block size
    size_t arraySz = static_cast<size_t>(this->_arraySz) + n + this->_schedBlkSz - 1;
    arraySz &= ~(this->_schedBlkSz - 1);

    if (arraySz >= UINT32_MAX) {
      // cannot have more than UINT32_MAX elements
        return true;
    }

  // allocate enough scheduler blocks to cover all of the allocated status/idx items
    uint32_t nSchedBlks = arraySz / this->_schedBlkSz;

  // grow the arrays
    uint8_t *status = static_cast<uint8_t *>(std::malloc (arraySz * sizeof(uint8_t)));
    sid_t *idx = static_cast<sid_t *>(std::malloc (arraySz * sizeof(sid_t)));
    sched_block *schedBlks = static_cast<sched_block *>(std::malloc(nSchedBlks * sizeof(sched_block)));
    if ((status == nullptr) || (idx == nullptr) || (schedBlks == nullptr)) {
        return true;
    }
    if (this->_arraySz > 0) {
        std::memcpy (status, this->_status, this->_arraySz * sizeof(uint8_t));
        std::memcpy (idx, this->_idx, this->_arraySz * sizeof(sid_t));
        std::memcpy (schedBlks, this->_schedBlks, this->_nSchedBlksAlloc * sizeof(sched_block));
      // free the old storage
        std::free (this->_status);
        std::free (this->_idx);
        std::free (this->_schedBlks);
    }

  // initialize new sched_blocks
    uint32_t blkIx = this->_nSchedBlksAlloc;
    index_t ix = blkIx * this->_schedBlkSz;
    for (; blkIx < nSchedBlks;  blkIx++) {
        schedBlks[blkIx]._start = ix;
        ix += this->_schedBlkSz;
        schedBlks[blkIx]._stop = ix;
        schedBlks[blkIx]._nStable = 0;
        schedBlks[blkIx]._nDead = this->_schedBlkSz;
    }

  // update pointers etc.
    this->_status = status;
    this->_idx = idx;
    this->_schedBlks = schedBlks;
    this->_arraySz = arraySz;
    this->_nSchedBlksAlloc = nSchedBlks;

    return false;
}

// a local copy of strand state for workers
struct worker_cache {
    typedef strand_array::strand_t strand_t;
    typedef strand_array::index_t index_t;
    typedef strand_array::sid_t sid_t;
    typedef strand_array::block_t block_t;
    typedef strand_array::sched_block sched_block;

    strand_array        *_sarray;       // pointer to global strand_array structure
    uint8_t             *_status;       // the array of status information for the strands
    sid_t               *_idx;          // array of strand indices for indirect state rep.
    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling
    atomic_uint32_t     *_nextBlkPtr;   // pointer to _nextSchedBlk
    uint32_t            _nStabilizing;  // count of strands run by this worker that stabilized in
                                        // the current superstep
#ifdef DIDEROT_HAS_STRAND_DIE
    uint32_t            _nDying;        // count of strands run by this worker that died in
                                        // the current superstep
#endif
    uint32_t            _nSchedBlks;    // number of scheduling blocks
    uint32_t            _schedBlkSz;    // size of scheduling blocks
#ifndef NDEBUG
    uint32_t            _nStrands;      // number of strands in the _idx and _status arrays
#endif
    block_t             _newBlock;      // strand-storage block for new strands
    strand_t            *_nextStrand;   // allocation pointer for new strands; should point inside
                                        // the _newBlock
    strand_t            *_limitPtr;     // limit pointer for new-strand allocation
    std::vector<sid_t>  _fresh;         // fresh strands created in current superstep

  // allocate a block of storage for new strands; returns true if there is an error
    bool alloc_block ();

    void init (strand_array &sarr)
    {
        this->_sarray = &sarr;
        this->_status = sarr._status;
        this->_idx = sarr._idx;
        this->_schedBlks = sarr._schedBlks;
        this->_nextBlkPtr = &sarr._nextSchedBlk;
        this->_nStabilizing = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
        this->_nDying = 0;
#endif
        this->_nSchedBlks = sarr._nSchedBlks;
        this->_schedBlkSz = sarr._schedBlkSz;
#ifndef NDEBUG
        this->_nStrands = sarr._nStrands;
#endif
        this->_nextStrand = nullptr;
        this->_limitPtr = nullptr;
        sarr._workers.push_back (this);
    }

  // refresh those parts of the cache that might change between steps
    void refresh ()
    {
        this->_status = this->_sarray->_status;
        this->_idx = this->_sarray->_idx;
        this->_nStabilizing = 0; /* QUESTION: is this the correct place for this? */
#ifdef DIDEROT_HAS_STRAND_DIE
        this->_nDying = 0;
#endif
        this->_schedBlks = this->_sarray->_schedBlks;
        this->_nSchedBlks = this->_sarray->_nSchedBlks;
#ifndef NDEBUG
        this->_nStrands = this->_sarray->_nStrands;
#endif
    }

  // return the ID of a strand, which is the value of the _idx array
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nStrands);
        return this->_idx[ix];
    }
  // direct indexing of strands by ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        return id;
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        assert (ix < this->_nStrands);
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }
  // return a pointer to the local state of strand ix
    @STRANDTY@ *local_state (index_t ix) const
    {
        return this->strand(ix);
    }
  // return a pointer to the local state of strand with the given ID
    @STRANDTY@ *id_to_local_state (sid_t id) const
    {
        return this->id_to_strand(id);
    }

#ifdef DIDEROT_HAS_START_METHOD
  // invoke strand's start method
    diderot::strand_status strand_start (@START_PARAMS@index_t ix)
    {
        return @STRAND@_start(@START_ARGS@this->strand(ix));
    }

    void run_start_methods (@START_PARAMS@sched_block *bp);
#endif // DIDEROT_HAS_START_METHOD

  // invoke strand's update method
    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)
    {
        return @STRAND@_update(@UPDATE_ARGS@this->strand(ix));
    }

  // invoke strand's stabilize method (multithread version)
    index_t strand_stabilize (sched_block *bp, @STABILIZE_PARAMS@index_t ix)
    {
#ifdef DIDEROT_HAS_STABILIZE_METHOD
        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));
#endif // DIDEROT_HAS_STABILIZE_METHOD
      // we swap the strand-indices at ix and bp->_start + bp->_nStable
        uint32_t jx = bp->_start + bp->_nStable;
        this->_status[jx] = diderot::kStable;
        std::swap (this->_idx[ix], this->_idx[jx]);
        bp->_nStable++;
        return ix+1;
    }

  // mark the given strand as dead (multithread version)
    index_t kill (sched_block *bp, index_t ix)
    {
        assert (bp->_start + bp->_nStable <= ix);
        assert (ix < bp->_start + bp->num_alive());
        bp->_nDead++;
      // swap the strand at ix with the last active strand in the block
        uint32_t jx = bp->_stop - bp->_nDead;
        this->_status[jx] = diderot::kDead;
        std::swap (this->_idx[ix], this->_idx[jx]);
        return ix;  // don't advance, since ix is an active strand after the swap
    }

  // return a pointer to the given newly allocated strand
    @STRANDTY@ *new_strand_state (index_t ix) const
    {
        return this->id_to_strand(this->_fresh[ix]);
    }

    index_t new_strand ()
    {
        index_t ix = this->_fresh.size();
        if (this->_nextStrand >= this->_limitPtr) {
            if (this->alloc_block()) {
                std::cerr << "Fatal error: unable to allocate space for new strands" << std::endl;
                exit (1);
            }
        }
        strand_t *strand = this->_nextStrand;
        this->_nextStrand++;
        this->_fresh.push_back (strand);
        new (strand) @STRANDTY@;
        return ix;
    }

  // iterator over active strands in a scheduling block
    index_t begin_active (const sched_block *bp) const { return bp->_start + bp->_nStable; }
    index_t end_active (const sched_block *bp) const { return bp->_stop - bp->_nDead; }
    index_t next_active (const sched_block *bp, index_t &ix) const { return ++ix; }

  // iterator over fresh strands in a scheduling block
    index_t begin_fresh (const sched_block *bp) const
    {
        index_t ix = this->begin_active(bp);
        while ((ix != this->end_active(bp)) && (this->status(ix) != diderot::kNew)) {
            ix = this->next_active(bp, ix);
        }
        return ix;
    }
    index_t end_fresh (const sched_block *bp) const { return this->end_active(bp); }
    index_t next_fresh (const sched_block *bp, index_t &ix) const
    {
        do {
            ix = this->next_active(bp, ix);
        } while ((ix != this->end_active(bp)) && (this->status(ix) != diderot::kNew));
        return ix;
    }

  // swap in and out states (NOP for this version)
    void swap () { }

  // get a block of strands
    sched_block *get_block ();

}; // struct worker_cache

strand_array::sched_block *worker_cache::get_block ()
{
    do {
        uint32_t blkId = this->_nextBlkPtr->fetch_add(1);
        if (blkId < this->_nSchedBlks) {
            strand_array::sched_block *bp = &this->_schedBlks[blkId];
            if (bp->num_active() > 0) {
                return bp;
            } // else skip stable block
        }
        else {  // no more blocks
            return nullptr;
        }
    } while (true);

}

bool worker_cache::alloc_block ()
{
    pthread_mutex_lock(&this->_sarray->_lock);
        char *blk = static_cast<block_t>(std::malloc (strand_array::BLKSZ * sizeof(@STRANDTY@)));
        if (blk == nullptr) {
            pthread_mutex_unlock(&this->_sarray->_lock);
            return true;
        }
        this->_sarray->_blocks.push_back(blk);
    pthread_mutex_unlock(&this->_sarray->_lock);

    this->_newBlock = blk;
    this->_nextStrand = reinterpret_cast<strand_t *>(blk);
    this->_limitPtr = reinterpret_cast<strand_t *>(blk + strand_array::BLKSZ * sizeof(@STRANDTY@));

    return false;
}

// finish the update phase of a superstep by compacting
// strands and including any fresh strands from the other
// workers.  Return true if there are any new or dead strands.
bool strand_array::finish_step ()
{
    int32_t nStabilizing = 0;
    int32_t nNew = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
    int32_t nDying = 0;
#endif

    int32_t blkIx = 0;
    for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {
        worker_cache *wp = *it;
        nStabilizing += wp->_nStabilizing;
        nNew += wp->_fresh.size();
#ifdef DIDEROT_HAS_STRAND_DIE
        nDying += wp->_nDying;
#endif
    }

    if (nNew > 0) {
      // the number of currently unused scheduler-block slots
        index_t nAvail = this->_schedBlkSz * this->_nSchedBlksAlloc
            - this->_nActive - this->_nStable;
        if (nAvail < nNew) {
          // we need to grow the _status, _idx, and _schedBlk arrays
            this->grow (nNew - nAvail);
        }
        assert (this->_arraySz == this->_nSchedBlksAlloc * this->_schedBlkSz);
#ifdef DIDEROT_HAS_STRAND_DIE
        this->_nStrands += nNew - nDying;
#else
        this->_nStrands += nNew;
#endif
      // copy fresh strands into the unused slots
        sched_block *bp = this->_schedBlks;
        index_t nextIx = bp->next_avail();
        uint32_t nBlks = 1;
        for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {
            worker_cache *wp = *it;
            for (auto jt = wp->_fresh.begin();  jt != wp->_fresh.end();  ++jt) {
              // advance to the next free slot
                while (nextIx == bp->_stop) {
                    bp++;
                    nBlks++;
                    nextIx = bp->next_avail();
                }
                assert (bp < this->_schedBlks + this->_nSchedBlksAlloc);
                this->_idx[nextIx] = *jt;
#ifdef DIDEROT_HAS_START_METHOD
                this->_status[nextIx] = diderot::kNew;
#else
                this->_status[nextIx] = diderot::kActive;
#endif
                nextIx++;
                bp->_nDead--;
            }
            wp->_fresh.clear();
        }
        this->_nSchedBlks = nBlks;
    }
#ifdef DIDEROT_HAS_STRAND_DIE
    else if (nDying > 0) {
      /* FIXME: compact dead strands */
/*
      // check to see if we need to compact dead strands?
        if ((this->_nStrands - this->_nActive) / this->_schedBlkSz > ??) {
        }
*/
    }
#endif

  // reset scheduler for next superstep
    this->_nextSchedBlk = 0;

  // update global count of stable strands
    this->_nStable += nStabilizing;
  // update global count of active strands
#ifdef DIDEROT_HAS_STRAND_DIE
    this->_nActive += nNew - (nStabilizing + nDying);

    return (nNew + nDying) > 0;
#else
    this->_nActive += nNew - nStabilizing;

    return nNew > 0;
#endif

}
/*---------- end par-sarr-indirect.in ----------*/
  D�/*---------- begin par-sarr.in ----------*/
// forward declaration of worker_cache type
struct worker_cache;
// forward declarations of strand methods
#ifdef DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_start (@START_PARAMS@@STRANDTY@ *self);
#endif // DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@@STRANDTY@ *self);
#ifdef DIDEROT_HAS_STABILIZE_METHOD
static void @STRAND@_stabilize (@STABILIZE_PARAMS@@STRANDTY@ *self);
#endif // DIDEROT_HAS_STABILIZE_METHOD

// strand_array for PARALLEL_TARGET/NO BSP/SINGLE STATE/DIRECT ACCESS
//
struct strand_array {
    typedef @STRANDTY@ strand_t;
    typedef uint32_t index_t;
    typedef index_t sid_t;              // strand ID (index into strand-state storage)

    // scheduling block of strands
    //
    struct CACHE_ALIGN sched_block {
        index_t         _start;         // first index in block
        index_t         _stop;          // last index in block + 1
        uint32_t        _nStable;       // number of stable strands in the block
        uint32_t        _nDead;         // number of dead strands in the block

      // return the number of strands in the block
        uint32_t num_strands () const { return this->_stop - this->_start; }
      // return the number of active strands in the block
        uint32_t num_active () const
        {
#ifdef DIDEROT_HAS_STRAND_DIE
            return this->num_strands() - (this->_nStable + this->_nDead);
#else
            return this->num_strands() - this->_nStable;
#endif
        }
    };

    uint8_t             *_status;       // the array of status information for the strands
    char                *_storage;      // points to array of @STRANDTY@ structs
    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling
    uint32_t            _nItems;        // number of items in the _storage and _status arrays
    uint32_t            _nStable;       // global number of stable strands
    uint32_t            _nActive;       // global number of active strands
    uint32_t            _nFresh;        // number of fresh strands (new strands from create_strands)
    uint32_t            _nSchedBlks;    // number of scheduling blocks
    uint32_t            _schedBlkSz;    // size of scheduling blocks
    atomic_uint32_t     _nextSchedBlk CACHE_ALIGN;
                                        // next block to schedule
    std::vector<worker_cache *> _workers;

    strand_array ()
        : _status(nullptr), _storage(nullptr), _schedBlks(nullptr), _nItems(0),
          _nStable(0), _nActive(0), _nFresh(0), _nSchedBlks(0), _schedBlkSz(0), _nextSchedBlk(0)
    { }
    ~strand_array ();

    uint32_t in_state_index () const { return 0; /* dummy */ }

    uint32_t num_active () const { return this->_nActive; }
    uint32_t num_stable () const { return this->_nStable; }
    uint32_t num_alive () const { return this->_nActive+this->_nStable; }

  // return the ID of a strand, which is just the value of the argument
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nItems);
        return ix;
    }
  // return a pointer to the strand with the given ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        assert (id < this->_nItems);
        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }
  // return a pointer to the local state of strand ix
    @STRANDTY@ *local_state (index_t ix) const
    {
        return this->strand(ix);
    }
  // return a pointer to the local state of strand with the given ID
    @STRANDTY@ *id_to_local_state (sid_t id) const
    {
        return this->id_to_strand(id);
    }

  // set the scheduling block size based on the number of workers and the number of
  // strands.  This should be called before alloc.
    void set_block_size (uint32_t nWorkers, uint32_t nStrands)
    {
        this->_schedBlkSz = diderot::sched_block_size (nWorkers, nStrands);
    }

  // allocate space for nItems organized into blkSz sized blocks of strands
    bool alloc (uint32_t nItems);

  // initialize the first nStrands locations as new active strands
    void create_strands (uint32_t nStrands);

  // swap in and out states (NOP for this version)
    void swap () { }

  // invoke strand's stabilize method (single-thread version)
    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)
    {
#ifdef DIDEROT_HAS_STABILIZE_METHOD
        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));
#endif // DIDEROT_HAS_STABILIZE_METHOD
        this->_status[ix] = diderot::kStable;
        this->_nActive--;
        this->_nStable++;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // mark the given strand as dead (single-thread version)
    index_t kill (index_t ix)
    {
        this->_status[ix] = diderot::kDead;
        this->_nActive--;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // prepare to run the workers
    void prepare_run ()
    {
        this->_nextSchedBlk = 0;
    }

#ifdef DIDEROT_BSP
  // finish the local-phase of a superstep; note that this function is only used
  // when BSP is enabled.
    bool finish_step ();
#endif

  // finish a kill_all operation (NOP)
    void finish_kill_all () { }

  // finish a stabilize_all operation (NOP)
    void finish_stabilize_all () { }

  // iterator over all alive strands (single-threaded version)
    index_t begin_alive () const
    {
        index_t ix = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }
    index_t end_alive () const { return this->_nItems; }
    index_t next_alive (index_t &ix) const
    {
        ix++;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }

  // iterator over all active strands (single-threaded version)
    index_t begin_active () const
    {
        index_t ix = 0;
        while ((ix < this->_nItems) && notActiveSts(this->status(ix))) {
            ix++;
        }
        return ix;
    }
    index_t end_active () const { return this->_nItems; }
    index_t next_active (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over stable strands
    index_t begin_stable () const
    {
        index_t ix = 0;
        while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable)) {
            ix++;
        }
        return ix;
    }
    index_t end_stable () const { return this->_nItems; }
    index_t next_stable (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable));
        return ix;
    }

  // iterator over fresh strands; since the only new strands were created by create_strand
  // we iterate over all of them
    index_t begin_fresh () const { return 0; }
    index_t end_fresh () const { return this->_nFresh; }
    index_t next_fresh (index_t &ix) const { return ++ix; }

}; // struct strand_array

strand_array::~strand_array ()
{
  // run destructors to reclaim any dynamic memory attached to the strand state
    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {
        this->strand(ix)->~@STRANDTY@();
    }
    if (this->_status != nullptr) std::free (this->_status);
    if (this->_storage != nullptr) std::free (this->_storage);
    if (this->_schedBlks != nullptr) std::free (this->_schedBlks);
}

bool strand_array::alloc (uint32_t nItems)
{
    if (this->_schedBlkSz == 0) {
        std::cerr << "Internal error: strand_array block size is 0\n";
        return true;
    }
    this->_storage = static_cast<char *>(std::malloc (nItems * sizeof(@STRANDTY@)));
    if (this->_storage == nullptr) {
        return true;
    }
    this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));
    if (this->_status == nullptr) {
        std::free (this->_storage);
        return true;
    }
    this->_nSchedBlks = (nItems + this->_schedBlkSz - 1) / this->_schedBlkSz;
    this->_schedBlks =
        static_cast<sched_block *>(std::malloc (this->_nSchedBlks * sizeof(sched_block)));
    if (this->_schedBlks == nullptr) {
        std::free (this->_storage);
        std::free (this->_status);
        return true;
    }
    this->_nItems = nItems;
    this->_nActive = 0;
    this->_nStable = 0;
    this->_nFresh = 0;
    return false;
}

void strand_array::create_strands (uint32_t nStrands)
{
    assert (this->_nActive == 0);
    assert (this->_nItems == nStrands);
    for (uint32_t ix = 0;  ix < nStrands;  ix++) {
#ifdef DIDEROT_HAS_START_METHOD
        this->_status[ix] = diderot::kNew;
#else
        this->_status[ix] = diderot::kActive;
#endif
        new(this->strand(ix)) @STRANDTY@;
    }
    this->_nActive = nStrands;
    this->_nFresh = nStrands;
  // initialize the scheduling blocks
    for (uint32_t ix = 0, i = 0;  i < this->_nSchedBlks;  i++) {
        this->_schedBlks[i]._start = ix;
        ix += this->_schedBlkSz;
        if (ix < nStrands) {
            this->_schedBlks[i]._stop = ix;
        }
        else {
            this->_schedBlks[i]._stop = nStrands;
        }
        this->_schedBlks[i]._nDead = 0;
        this->_schedBlks[i]._nStable = 0;
    }
}

// a local copy of strand state for workers
struct worker_cache {
    typedef strand_array::strand_t strand_t;
    typedef strand_array::index_t index_t;
    typedef strand_array::sid_t sid_t;
    typedef strand_array::sched_block sched_block;

    uint8_t             *_status;       // the array of status information for the strands
    char                *_storage;      // points to array of @STRANDTY@ structs
    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling
    atomic_uint32_t     *_nextBlkPtr;   // pointer to _nextSchedBlk
    uint32_t            _nStabilizing;  // count of strands run by this worker that stabilized in
                                        // the current superstep
#ifdef DIDEROT_HAS_STRAND_DIE
    uint32_t            _nDying;        // count of strands run by this worker that died in
                                        // the current superstep
#endif
    uint32_t            _nSchedBlks;    // number of scheduling blocks
    uint32_t            _schedBlkSz;    // size of scheduling blocks
#ifndef NDEBUG
    uint32_t        _nItems;            // number of items in the _storage and _status arrays
#endif

    void init (strand_array &sarr)
    {
        this->_status = sarr._status;
        this->_storage = sarr._storage;
        this->_schedBlks = sarr._schedBlks;
        this->_nextBlkPtr = &sarr._nextSchedBlk;
        this->_nSchedBlks = sarr._nSchedBlks;
        this->_schedBlkSz = sarr._schedBlkSz;
#ifndef NDEBUG
        this->_nItems = sarr._nItems;
#endif
        sarr._workers.push_back (this);
    }

  // refresh those parts of the cache that might change between steps
    void refresh ()
    {
        // this target does not support dynamic strands, so nothing can change
    }

  // return the ID of a strand, which is the value of the _idx array
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nItems);
        return ix;
    }
  // return a pointer to the strand with the given ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));
    }
  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }
  // return a pointer to the local state of strand ix
    @STRANDTY@ *local_state (index_t ix) const
    {
        return this->strand(ix);
    }
  // return a pointer to the local state of strand with the given ID
    @STRANDTY@ *id_to_local_state (sid_t id) const
    {
        return this->id_to_strand(id);
    }

#ifdef DIDEROT_HAS_START_METHOD
  // invoke strand's start method
    diderot::strand_status strand_start (@START_PARAMS@index_t ix)
    {
        return @STRAND@_start(@START_ARGS@this->strand(ix));
    }

    void run_start_methods (@START_PARAMS@sched_block *bp);
#endif // DIDEROT_HAS_START_METHOD

  // invoke strand's update method
    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)
    {
        return @STRAND@_update(@UPDATE_ARGS@this->strand(ix));
    }

  // invoke strand's stabilize method (multithread version)
    index_t strand_stabilize (sched_block *bp, @STABILIZE_PARAMS@index_t ix)
    {
#ifdef DIDEROT_HAS_STABILIZE_METHOD
        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));
#endif // DIDEROT_HAS_STABILIZE_METHOD
        this->_status[ix] = diderot::kStable;
        bp->_nStable++;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));
        return ix;
    }

  // mark the given strand as dead (multithread version)
    index_t kill (sched_block *bp, index_t ix)
    {
        this->_status[ix] = diderot::kDead;
        bp->_nDead++;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over alive strands in a scheduling block
    index_t begin_alive (const sched_block *bp) const
    {
        index_t ix = bp->_start;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < bp->_stop) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }
    index_t end_alive (const sched_block *bp) const { return bp->_stop; }
    index_t next_alive (const sched_block *bp, index_t &ix) const
    {
#ifdef DIDEROT_HAS_STRAND_DIE
        do {
            ix++;
        } while ((ix < bp->_stop) && notAliveSts(this->status(ix)));
#endif
        return ix;
    }

  // iterator over active strands in a scheduling block
    index_t begin_active (const sched_block *bp) const
    {
        index_t ix = bp->_start;
        while ((ix < bp->_stop) && notActiveSts(this->status(ix))) {
            ix++;
        }
        return ix;
    }
    index_t end_active (const sched_block *bp) const { return bp->_stop; }
    index_t next_active (const sched_block *bp, index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over fresh strands in a scheduling block
    index_t begin_fresh (const sched_block *bp) const
    {
        index_t ix = bp->_start;
        while ((ix < bp->_stop) && (this->status(ix) != diderot::kNew)) {
            ix++;
        }
        return ix;
    }
    index_t end_fresh (const sched_block *bp) const { return bp->_stop; }
    index_t next_fresh (const sched_block *bp, index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < bp->_stop) && (this->status(ix) != diderot::kNew));
        return ix;
    }

  // swap in and out states (NOP for this version)
    void swap () { }

  // get a block of strands
    sched_block *get_block ();

}; // struct worker_cache

strand_array::sched_block *worker_cache::get_block ()
{
    do {
        uint32_t blkId = this->_nextBlkPtr->fetch_add(1);
        if (blkId < this->_nSchedBlks) {
            strand_array::sched_block *bp = &this->_schedBlks[blkId];
            if (bp->num_active() > 0) {
                return bp;
            } // else skip stable block
        }
        else {  // no more blocks
            return nullptr;
        }
    } while (true);

}

#ifdef DIDEROT_BSP
// finish the update phase of a superstep.  Return true if there are any dead strands.
bool strand_array::finish_step ()
{
    int32_t nStabilizing = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
    int32_t nDying = 0;
#endif

    for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {
        worker_cache *wp = *it;
        nStabilizing += wp->_nStabilizing;
#ifdef DIDEROT_HAS_STRAND_DIE
        nDying += wp->_nDying;
#endif
    }

#ifdef DIDEROT_HAS_STRAND_DIE
    if (nDying > 0) {
      /* FIXME: compact dead strands */
/*
      // check to see if we need to compact dead strands?
        if ((this->_nStrands - this->_nActive) / this->_schedBlkSz > ??) {
        }
*/
    }
#endif

  // reset scheduler for next superstep
    this->_nextSchedBlk = 0;

  // update global count of stable strands
    this->_nStable += nStabilizing;
  // update global count of active strands
#ifdef DIDEROT_HAS_STRAND_DIE
    this->_nActive -= (nStabilizing + nDying);

    return (nDying > 0);
#else
    this->_nActive -= nStabilizing;

    return false;
#endif

}
#endif // DIDEROT_BSP
/*---------- end par-sarr.in ----------*/
  /*---------- begin seq-main.in ----------*/
using namespace @PREFIX@;

//! Main function for standalone sequential C target
//
int main (int argc, const char **argv)
{
    bool        timingFlg = false;      //! true if timing computation
    uint32_t    stepLimit = 0;          //! limit on number of execution steps (0 means unlimited)
    std::string printFile = "-";        //! file to direct printed output into
#ifdef DIDEROT_EXEC_SNAPSHOT
    uint32_t    snapshotPeriod = 0;     //! supersteps per snapshot; 0 means no snapshots
#endif
    uint32_t    nSteps = 0;             //! number of supersteps taken

  // create the world
    world *wrld = new (std::nothrow) world();
    if (wrld == nullptr) {
        std::cerr << "unable to create world" << std::endl;
        exit(1);
    }

#ifndef DIDEROT_NO_INPUTS
  // initialize the default values for the inputs
    cmd_line_inputs inputs;
    init_defaults (&inputs);
#endif

  // handle command-line options
    {
        diderot::options *opts = new diderot::options ();
        opts->add ("l,limit", "specify limit on number of super-steps (0 means unlimited)",
            &stepLimit, true);
#ifdef DIDEROT_EXEC_SNAPSHOT
        opts->add ("s,snapshot",
            "specify number of super-steps per snapshot (0 means no snapshots)",
            &snapshotPeriod, true);
#endif
        opts->add ("print", "specify where to direct printed output", &printFile, true);
        opts->addFlag ("v,verbose", "enable runtime-system messages", &(wrld->_verbose));
        opts->addFlag ("t,timing", "enable execution timing", &timingFlg);
#ifndef DIDEROT_NO_INPUTS
      // register options for setting global inputs
        register_inputs (&inputs, opts);
#endif
        register_outputs (opts);
        opts->process (argc, argv);
        delete opts;
    }

  // redirect printing (if necessary)
    if (printFile.compare("-") != 0) {
        wrld->_printTo = new std::ofstream (printFile);
        if (wrld->_printTo->fail()) {
            std::cerr << "Error opening print file" << std::endl;
            delete wrld;
            exit(1);
        }
    }

  // initialize scheduler stuff
    if (wrld->_verbose) {
        std::cerr << "initializing world ..." << std::endl;
    }
    if (wrld->init()) {
        std::cerr << "Error initializing world:\n" << wrld->get_errors() << std::endl;
        delete wrld;
        exit(1);
    }

#ifndef DIDEROT_NO_INPUTS
  // initialize the input globals
    if (init_inputs (wrld, &inputs)) {
        std::cerr << "Error initializing inputs:\n" << wrld->get_errors() << std::endl;
        delete wrld;
        exit(1);
    }
#endif

  // run the generated global initialization code
    if (wrld->_verbose) {
        std::cerr << "initializing globals and creating strands ...\n";
    }
    if (wrld->create_strands()) {
        std::cerr << "Error in global initialization:\n"
            << wrld->get_errors() << std::endl;
        delete wrld;
        exit(1);
    }

#ifdef DIDEROT_EXEC_SNAPSHOT

    if (snapshotPeriod > 0) {
     // write initial state as snapshot 0
        write_snapshot (wrld, "-0000");
     // run the program for `snapshotPeriod` steps at a time with a snapshot after each run
        while (true) {
            uint32_t n, limit;
          // determine a step limit for the next run
            if (stepLimit > 0) {
                if (stepLimit <= nSteps) {
                    break;
                }
                limit = std::min(stepLimit - nSteps, snapshotPeriod);
            }
            else {
                limit = snapshotPeriod;
            }
          // run the program for upto limit steps
            if ((n = wrld->run (limit)) == 0) {
                break;
            }
            nSteps += n;
            if ((wrld->_errors->errNum > 0) || (wrld->_strands.num_alive() == 0)) {
                break;
            }
          // write a snapshot with the step count as a suffix
            std::string suffix = std::to_string(nSteps);
            if (suffix.length() < 4) {
                suffix = std::string("0000").substr(0, 4 - suffix.length()) + suffix;
            }
            suffix = "-" + suffix;
            write_snapshot (wrld, suffix);
        }
    }
    else {
        nSteps = wrld->run (stepLimit);
    }

#else // !DIDEROT_EXEC_SNAPSHOT

    nSteps = wrld->run (stepLimit);

#endif // DIDEROT_EXEC_SNAPSHOT

    if (wrld->_errors->errNum > 0) {
        std::cerr << "Error during execution:\n" << wrld->get_errors() << std::endl;
        delete wrld;
        exit(1);
    }

    if ((stepLimit != 0) && (wrld->_strands.num_active() > 0)) {
#ifdef DIDEROT_STRAND_ARRAY
        if (wrld->_verbose) {
            std::cerr << "Step limit expired; "
                << wrld->_strands.num_active() << " active strands remaining" << std::endl;
        }
#else
      // step limit expired, so kill remaining strands
        if (wrld->_verbose) {
            std::cerr << "Step limit expired. Killing remaining "
                << wrld->_strands.num_active() << " active strands" << std::endl;
        }
        wrld->kill_all();
#endif
    }

    if (wrld->_verbose) {
        std::cerr << "done: " << nSteps << " steps, in " << wrld->_run_time << " seconds";
#ifndef DIDEROT_STRAND_ARRAY
        std::cerr << "; " << wrld->_strands.num_stable() << " stable strands" << std::endl;
#else
        std::cerr << std::endl;
#endif
    }
    else if (timingFlg) {
        std::cout << "usr=" << wrld->_run_time << std::endl;
    }

  // output the final strand states
    write_output (wrld);

    delete wrld;

    return 0;

} // main
/*---------- end seq-main.in ----------*/
  /*---------- begin seq-run-nobsp.in ----------*/
//! Run the Diderot program (sequential version without BSP semantics)
//! \param max_nsteps the limit on the number of super steps; 0 means unlimited
//! \return the number of steps taken, or 0 on error.
uint32_t world::run (uint32_t max_nsteps)
{
    if (this->_stage == diderot::POST_CREATE) {
#ifdef DIDEROT_HAS_GLOBAL_START
        this->global_start();
#endif
        this->_stage = diderot::RUNNING;
    }
    assert (this->_stage == diderot::RUNNING);

#ifndef DIDEROT_NO_GLOBALS
    globals *glob = this->_globals;
#endif

    if (max_nsteps == 0) {
        max_nsteps = 0xffffffff;  // essentially unlimited
    }

    double t0 = airTime();

    if (this->_verbose) {
        std::cerr << "run with " << this->_strands.num_alive() << " strands ..." << std::endl;
    }

#ifdef DIDEROT_HAS_START_METHOD
    this->run_start_methods();
#endif

  // iterate until all strands are stable
    uint32_t maxSteps = 0;
    for (auto ix = this->_strands.begin_active();
         ix != this->_strands.end_active();
         )
    {
        diderot::strand_status sts = this->_strands.status(ix);
        uint32_t nSteps = 0;
        while ((! sts) && (nSteps < max_nsteps)) {
            nSteps++;
            sts = this->_strands.strand_update(@UPDATE_ARGS_IN_WRLD@ix);
        }
        switch (sts) {
          case diderot::kStabilize:
          // stabilize the strand's state.
            ix = this->_strands.strand_stabilize (@STABILIZE_ARGS_IN_WRLD@ix);
            break;
#ifdef DIDEROT_HAS_STRAND_DIE
          case diderot::kDie:
            ix = this->_strands.kill (ix);
            break;
#endif
          default:
            assert (sts == this->_strands.status(ix));
	    ix = this->_strands.next_active(ix);
            break;
        }
        if (maxSteps < nSteps) maxSteps = nSteps;
    }

    this->_run_time += airTime() - t0;

    if (this->_strands.num_active() == 0)
        this->_stage = diderot::DONE;

    return maxSteps;

} // world::run
/*---------- end seq-run-nobsp.in ----------*/
  
a/*---------- begin seq-run.in ----------*/
//! Run the Diderot program (sequential version)
//! \param max_nsteps the limit on the number of super steps; 0 means unlimited
//! \return the number of steps taken, or -1 on error.
uint32_t world::run (uint32_t max_nsteps)
{
    if (this->_stage == diderot::POST_CREATE) {
#ifdef DIDEROT_HAS_GLOBAL_START
        this->global_start();
#endif
        this->_stage = diderot::RUNNING;
    }
    assert (this->_stage == diderot::RUNNING);

#ifndef DIDEROT_NO_GLOBALS
    globals *glob = this->_globals;
#endif

    if (max_nsteps == 0) {
        max_nsteps = 0xffffffff;  // essentially unlimited
    }

    double t0 = airTime();

    if (this->_verbose) {
        std::cerr << "run with " << this->_strands.num_active() << " strands ..." << std::endl;
    }

#if defined(DIDEROT_HAS_STRAND_COMMUNICATION) && !(defined(DIDEROT_HAS_STRAND_DIE) || defined(DIDEROT_HAS_STRAND_NEW))
  // initial recording of strands for KD-tree
    this->_tree->update_strands ();
#endif

  // iterate until all strands are stable
    bool treeNeedsUpdate = true;
    uint32_t nSteps = 0;
    while ((this->_strands.num_active() > 0) && (nSteps < max_nsteps)) {
        nSteps++;
#ifdef DIDEROT_HAS_STRAND_COMMUNICATION
      // build spatial partition to support communication
        if (treeNeedsUpdate) {
	    this->_tree->update_strands ();
        }
        this->_tree->rebuild ();
#endif
#ifdef DIDEROT_HAS_START_METHOD
      // run start methods for fresh strands
        this->run_start_methods();
#endif
      // update strands
        for (auto ix = this->_strands.begin_active();
            ix != this->_strands.end_active();
            )
        {
            diderot::strand_status sts = this->_strands.strand_update (@UPDATE_ARGS_IN_WRLD@ix);
            switch (sts) {
              case diderot::kStabilize:
                ix = this->_strands.strand_stabilize(@STABILIZE_ARGS_IN_WRLD@ix);
                break;
#ifdef DIDEROT_HAS_STRAND_DIE
              case diderot::kDie:
                ix = this->_strands.kill(ix);
                break;
#endif
              default:
                ix = this->_strands.next_active(ix);
                break;
            }
        }
      // finish the local-phase of the superstep by updating strand status
        treeNeedsUpdate = this->_strands.finish_step();

        this->swap_state();

#ifdef DIDEROT_HAS_GLOBAL_UPDATE
        this->global_update();
#endif
    }

    this->_run_time += airTime() - t0;

    if (this->_strands.num_active() == 0)
        this->_stage = diderot::DONE;

    return nSteps;

} // world::run
/*---------- end seq-run.in ----------*/
  �/*---------- begin seq-run-start.in ----------*/
// Run the start methods of the initial strands (sequential version)
//
void world::run_start_methods ()
{
#ifndef DIDEROT_NO_GLOBALS
    globals *glob = this->_globals;
#endif

    for (auto ix = this->_strands.begin_fresh();
        ix != this->_strands.end_fresh();
        )
    {
        diderot::strand_status sts = this->_strands.strand_start(@START_ARGS_IN_WRLD@ix);
        switch (sts) {
          case diderot::kStabilize:
            ix = this->_strands.strand_stabilize (@STABILIZE_ARGS_IN_WRLD@ix);
            break;
#ifdef DIDEROT_HAS_STRAND_DIE
          case diderot::kDie:
            ix = this->_strands.kill (ix);
            break;
#endif
          default:
            assert (sts == this->_strands.status(ix));
            ix = this->_strands.next_fresh(ix);
            break;
        }
    }
    this->_strands._nFresh = 0;

}
/*---------- end seq-run-start.in ----------*/
  :P/*---------- begin seq-sarr-dual-indirect.in ----------*/
// forward declarations of strand methods
#ifdef DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_start (@START_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#endif // DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#ifdef DIDEROT_HAS_STABILIZE_METHOD
static void @STRAND@_stabilize (@STABILIZE_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#endif // DIDEROT_HAS_STABILIZE_METHOD

// strand_array for BSP/DUAL STATE/INDIRECT ACCESS
//
struct strand_array {
    typedef @STRANDTY@ strand_t;
    typedef uint32_t index_t;           // strand index (index into _idx and _status arrays)
    typedef uint32_t sid_t;             // strand ID (index into strand-state storage)
    typedef char *block_t;              // points to array of @STRANDTY@ structs

    uint8_t             *_status;       // the array of status information for the strands
    uint32_t            *_idx;          // array of strand indices for indirect state rep.
    std::vector<block_t> _blocks;       // vector of pointers to strand-storage blocks
    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)
    uint32_t            _nItems;        // number of items in the _blocks and _status arrays
    uint32_t            _nStable;       // stable strands (in locations 0.._nStable-1)
    uint32_t            _nActive;       // active strands (in locations _nStable.._nStable+_nActive-1)
    uint32_t            _nStabilizing;  // number of stablizing strands
    uint32_t            _nDying;        // number of dying strands
    uint32_t            _nNew;          // number of new strands
    uint32_t            _nFresh;        // number of fresh strands (new strands from previous step)

    static const uint32_t LOG_BLKSZ = 12;               // 2^12 items per block
    static const uint32_t BLKSZ = (1 << LOG_BLKSZ);
    static const uint32_t BLKMASK = (BLKSZ-1);          // mask for block index

    strand_array () : _status(nullptr), _idx(nullptr), _nItems(0) { }
    ~strand_array ();

    uint32_t in_state_index () const { return this->_inIdx; }

    uint32_t num_active () const { return this->_nActive; }
    uint32_t num_stable () const { return this->_nStable; }
    uint32_t num_alive () const { return this->_nActive+this->_nStable; }
    uint32_t num_fresh () const { return this->_nFresh; }

  // return the ID of a strand, which is the value of the _idx array
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nItems);
        return this->_idx[ix];
    }
  // return a pointer to the strand with the given ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        assert (id < this->_nItems);
        uint32_t blkId = id >> LOG_BLKSZ;
        uint32_t offset = id & BLKMASK;
        return reinterpret_cast<@STRANDTY@ *>(this->_blocks[blkId] + offset * sizeof(@STRANDTY@));
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        assert (ix < this->_nItems);
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }

  // return a pointer to the local state of strand ix
    @STRAND@_local *local_state (index_t ix) const
    {
        return &(this->strand(ix)->_local);
    }
  // return a pointer to the local state of strand with the given ID
    @STRAND@_local *id_to_local_state (sid_t id) const
    {
        return &(this->id_to_strand(id)->_local);
    }
  // return a pointer to the in-state of strand ix
    const @STRAND@_shared *in_state (index_t ix) const
    {
        return &(this->strand(ix)->_shared[this->_inIdx]);
    }
  // return a pointer to the in-state of the strand with the given ID
    const @STRAND@_shared *id_to_in_state (sid_t id) const
    {
        return &(this->id_to_strand(id)->_shared[this->_inIdx]);
    }
  // return a pointer to the out-state of strand ix
    @STRAND@_shared *out_state (index_t ix) const
    {
        return &(this->strand(ix)->_shared[this->_inIdx ^ 1]);
    }

  // wrappers for accessing the state of newly created strands
    @STRAND@_local *new_local_state (index_t ix) const
    {
        return this->local_state(ix);
    }
    @STRAND@_shared *new_out_state (index_t ix) const
    {
        return this->out_state(ix);
    }

  // is an index valid for the strand array?
    bool validIndex (index_t ix) const { return (ix < this->_nItems); }

  // is a given strand alive?
    bool isAlive (index_t ix) const
    {
#ifdef DIDEROT_HAS_STRAND_DIE
        return aliveSts(this->status(ix));
#else
        return true;
#endif
    }

  // deallocate space reserved for strands
    void dealloc ();

  // allocate space for at least nItems
    bool alloc (uint32_t nItems)
    {
        nItems = (nItems + BLKSZ - 1) & ~BLKMASK;
        uint32_t nBlks = nItems >> LOG_BLKSZ;
      // allocate block vector
        this->_blocks.resize(nBlks, nullptr);
      // allocate blocks
        for (int i = 0;  i < nBlks;  i++) {
            this->_blocks[i] = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));
            if (this->_blocks[i]  == nullptr) {
              // unable to allocate memory
                this->dealloc();
                return true;
            }
        }
      // allocate _status array
        this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));
        if (this->_status == nullptr) {
            this->dealloc();
            return true;
        }
      // allocate _idx array
        this->_idx = static_cast<uint32_t *>(std::malloc (nItems * sizeof(uint32_t)));
        if (this->_idx == nullptr) {
            this->dealloc();
            return true;
        }
      // initialize arrays
        for (index_t ix = 0;  ix < nItems;  ix++) {
            this->_status[ix] = diderot::kDead;
            this->_idx[ix] = ix;
        }
        this->_inIdx = 0;
        this->_nItems = nItems;
        this->_nActive = 0;
        this->_nStable = 0;
        this->_nStabilizing = 0;
        this->_nNew = 0;
        this->_nDying = 0;
        this->_nFresh = 0;
        return false;
    }

  // initialize the first nStrands locations as new active strands
    void create_strands (uint32_t nStrands)
    {
        assert (this->_nActive == 0);
        assert (this->_nItems >= nStrands);
        for (index_t ix = 0;  ix < nStrands;  ix++) {
            this->_status[ix] = diderot::kActive;
            new (this->strand(ix)) @STRANDTY@;
        }
        this->_nActive = nStrands;
        this->_nFresh = nStrands;
    }

  // swap in and out states
    void swap ()
    {
        this->_inIdx ^= 1;
    }

#ifdef DIDEROT_HAS_START_METHOD
  // invoke strand's start method
    diderot::strand_status strand_start (@START_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        return @STRAND@_start (@START_ARGS@
            &self->_local,
            &self->_shared[this->_inIdx],
            &self->_shared[this->_inIdx^1]);
    }
#endif // DIDEROT_HAS_START_METHOD

  // invoke strand's update method
    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        return @STRAND@_update (@UPDATE_ARGS@
            &self->_local,
            &self->_shared[this->_inIdx],
            &self->_shared[this->_inIdx^1]);
    }

  // invoke strand's stabilize method
    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];
        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];
#ifdef DIDEROT_HAS_STABILIZE_METHOD
      // note that we swap out and in here because out holds the current state
        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);
        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));
#else
        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));
#endif // DIDEROT_HAS_STABILIZE_METHOD
      // we swap the strand-indices at ix and _nStable + this->_nStabilizing
        uint32_t jx = this->_nStable + this->_nStabilizing;
        this->_status[jx] = diderot::kStabilize;
        std::swap (this->_idx[ix], this->_idx[jx]);
        this->_nStabilizing++;
        return ix+1;
    }

  // record that the specified strand is dying
    index_t kill (index_t ix)
    {
        assert (this->_nStable <= ix);
        assert (ix < this->num_alive());
        this->_nDying++;
        uint32_t jx = this->num_alive() - this->_nDying;
        this->_status[jx] = diderot::kDie;
        std::swap (this->_idx[ix], this->_idx[jx]);
        return ix;  // don't advance, since ix is an active strand after the swap
    }

  // allocate a new strand
    index_t new_strand ()
    {
        index_t ix = this->num_alive() + this->_nNew;
        if (this->_nItems <= ix) {
            if (this->grow ()) {
                std::cerr << "Fatal error: unable to allocate space for new strands" << std::endl;
                exit (1);
            }
        }
        this->_status[ix] = diderot::kNew;
        new (this->strand(ix)) @STRANDTY@;
        this->_nNew++;
        return ix;
    }

  // finish a step by updating the strand statuses and the various counters
    bool finish_step ()
    {
        bool anyNewDie = ((this->_nDying + this->_nNew) > 0);
        index_t next = this->_nStable;
        for (index_t ix = 0;  ix < this->_nStabilizing;  ix++, next++) {
            this->_status[next] = diderot::kStable;
        }
        if (this->_nDying == 0) {
          // no need to swap strands
            index_t next = this->num_alive();
            for (auto ix = 0;  ix < this->_nNew;  ix++, next++) {
                this->_status[next] = diderot::kActive;
            }
        }
        else {
          // first handle the dying strands
            next = this->num_alive() - this->_nDying;
            for (index_t ix = 0;  ix < this->_nDying;  ix++, next++) {
                this->_status[next] = diderot::kDead;
              // invoke the dead strand's destructors
                reinterpret_cast<@STRANDTY@ *>(this->strand(next))->~@STRANDTY@();
            }
          // move the new strands down over the dying strands
            index_t src = this->num_alive();
            index_t dst = src - this->_nDying;
            for (auto ix = 0;  ix < this->_nNew;  ix++, dst++, src++) {
                this->_status[dst] = diderot::kActive;
                this->_status[src] = diderot::kDead;
                std::swap (this->_idx[src], this->_idx[dst]);
            }
        }

      // update counts
        this->_nFresh = this->_nNew;
        this->_nStable += this->_nStabilizing;
        this->_nActive -= this->_nStabilizing + this->_nDying;
        this->_nActive += this->_nNew;
        this->_nStabilizing = 0;
        this->_nNew = 0;
        this->_nDying = 0;

        return anyNewDie;
    }

  // finish a kill_all operation
    void finish_kill_all ()
    {
        this->_nActive -= this->_nDying;
        this->_nDying = 0;
    }

  // finish a stabilize_all operation
    void finish_stabilize_all ()
    {
        this->_nStable += this->_nStabilizing;
        this->_nActive -= this->_nStabilizing;
        this->_nStabilizing = 0;
    }

  // iterator over stable strands
    index_t begin_stable () const { return 0; }
    index_t end_stable () const { return this->_nStable; }
    index_t next_stable (index_t &ix) const { return ++ix; }

  // iterator over active strands
    index_t begin_active () const { return this->_nStable+this->_nStabilizing; }
    index_t end_active () const { return this->_nStable+this->_nActive-this->_nDying; }
    index_t next_active (index_t &ix) const { return ++ix; }

  // iterator over alive (active+stable) strands; we assume that _nStabilizing and _nNew are 0
    index_t begin_alive () const { return 0; }
    index_t end_alive () const { return this->num_alive(); }
    index_t next_alive (index_t &ix) const { return ++ix; }

  // iterator over fresh strands
    index_t begin_fresh () const { return this->num_alive() - this->_nFresh; }
    index_t end_fresh () const { return this->num_alive(); }
    index_t next_fresh (index_t &ix) const { return ++ix; }

  // allocate more space for strand state; return true on error
    bool grow ()
    {
        size_t nItems = static_cast<size_t>(this->_nItems) + BLKSZ;
        if (nItems >= UINT32_MAX) {
          // cannot have more than UINT32_MAX elements
            return true;
        }

      // allocate a new block at the end of the _blocks array
        char *blk = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));
        if (blk == nullptr) {
            return true;
        }
        this->_blocks.push_back (blk);

      // grow the _status and _idx arrays
        uint8_t *status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));
        uint32_t *idx = static_cast<uint32_t *>(std::malloc (nItems * sizeof(uint32_t)));
        if ((status == nullptr) || (idx == nullptr)) {
            return true;
        }
        std::memcpy (status, this->_status, this->_nItems * sizeof(uint8_t));
        std::memcpy (idx, this->_idx, this->_nItems * sizeof(uint32_t));

      // initialize the new storage
        @STRANDTY@ *p = reinterpret_cast<@STRANDTY@ *>(blk);
        for (uint32_t ix = this->_nItems;  ix < nItems;  ix++) {
            status[ix] = diderot::kDead;
            idx[ix] = ix;
        }

      // free the old storage
        std::free (this->_status);
        std::free (this->_idx);

      // update pointers
        this->_status = status;
        this->_idx = idx;
        this->_nItems = nItems;

        return false;
    }

}; // struct strand_array

strand_array::~strand_array ()
{
  // run destructors to reclaim any dynamic memory attached to the strand state
    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {
        this->strand(ix)->~@STRANDTY@();
    }
    this->dealloc();
}

void strand_array::dealloc ()
{
    if (this->_status != nullptr) {
        std::free (this->_status);
        this->_status = nullptr;
    }
    if (this->_idx != nullptr) {
        std::free (this->_idx);
        this->_idx = nullptr;
    }
    for (uint32_t i = 0;  i < this->_blocks.size();  i++) {
        if (this->_blocks[i] != nullptr) {
            std::free (this->_blocks[i]);
            this->_blocks[i] = nullptr;
        }
        else {
            break;
        }
    }
}
/*---------- end seq-sarr-dual-indirect.in ----------*/
  &�/*---------- begin seq-sarr-dual.in ----------*/
// forward declarations of strand methods
#ifdef DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_start (@START_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#endif // DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#ifdef DIDEROT_HAS_STABILIZE_METHOD
static void @STRAND@_stabilize (@STABILIZE_PARAMS@
    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);
#endif // DIDEROT_HAS_STABILIZE_METHOD

// if we have both communication and "die", then we need to track when strands die
// so that we can rebuild the list of strands use to construct the kd-tree
#if defined(DIDEROT_HAS_STRAND_COMMUNICATION) && !defined(DIDEROT_HAS_STRAND_DIE)
#  define TRACK_STRAND_DEATH
#endif

// strand_array for SEQUENTIAL/BSP/DUAL STATE/DIRECT ACCESS
//
struct strand_array {
    typedef @STRANDTY@ strand_t;
    typedef uint32_t index_t;
    typedef index_t sid_t;              // strand ID (index into strand-state storage)

    uint8_t             *_status;       // the array of status information for the strands
    char                *_storage;      // points to array of @STRANDTY@ structs
    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)
    uint32_t            _nItems;        // number of items in the _storage and _status arrays
    uint32_t            _nStable;       // number of stable strands
    uint32_t            _nActive;       // number of active strands
    uint32_t            _nFresh;        // number of fresh strands (new strands from create_strands)
#ifdef TRACK_STRAND_DEATH
    bool                _died;          // a strand died in the current superstep.
#endif

    strand_array () : _status(nullptr), _storage(nullptr), _nItems(0) { }
    ~strand_array ();

    uint32_t in_state_index () const { return this->_inIdx; }

    uint32_t num_active () const { return this->_nActive; }
    uint32_t num_stable () const { return this->_nStable; }
    uint32_t num_alive () const { return this->_nActive+this->_nStable; }

  // return the ID of a strand, which is the same as the ix index
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nItems);
        return ix;
    }
  // return a pointer to the strand with the given ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        assert (id < this->_nItems);
        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        assert (ix < this->_nItems);
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }
  // return a pointer to the local state of strand ix
    @STRAND@_local *local_state (index_t ix) const
    {
        return &(this->strand(ix)->_local);
    }
  // return a pointer to the local state of strand with the given ID
    @STRAND@_local *id_to_local_state (sid_t id) const
    {
        return &(this->id_to_strand(id)->_local);
    }
  // return a pointer to the in-state of strand ix
    const @STRAND@_shared *in_state (index_t ix) const
    {
        return &(this->strand(ix)->_shared[this->_inIdx]);
    }
  // return a pointer to the in-state of the strand with the given ID
    const @STRAND@_shared *id_to_in_state (sid_t id) const
    {
        return &(this->id_to_strand(id)->_shared[this->_inIdx]);
    }
  // return a pointer to the out-state of strand ix
    @STRAND@_shared *out_state (index_t ix) const
    {
        return &(this->strand(ix)->_shared[this->_inIdx ^ 1]);
    }

  // is an index valid for the strand array?
    bool validIndex (index_t ix) const { return (ix < this->_nItems); }

  // is a given strand alive?
    bool isAlive (index_t ix) const
    {
#ifdef DIDEROT_HAS_STRAND_DIE
        return aliveSts(this->status(ix));
#else
        return true;
#endif
    }

  // allocate space for nItems
    bool alloc (uint32_t nItems)
    {
        this->_storage = static_cast<char *>(std::malloc (nItems * sizeof(@STRANDTY@)));
        if (this->_storage == nullptr) {
            return true;
        }
        this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));
        if (this->_status == nullptr) {
            std::free (this->_storage);
            return true;
        }
        this->_inIdx = 0;
        this->_nItems = nItems;
        this->_nActive = 0;
        this->_nStable = 0;
        this->_nFresh = 0;
        return false;
    }

  // initialize the first nStrands locations as new active strands
    void create_strands (uint32_t nStrands)
    {
        assert (this->_nActive == 0);
        assert (this->_nItems == nStrands);
        for (index_t ix = 0;  ix < nStrands;  ix++) {
            this->_status[ix] = diderot::kActive;
            new (this->strand(ix)) @STRANDTY@;
        }
        this->_nActive = nStrands;
        this->_nFresh = nStrands;
#ifdef TRACK_STRAND_DEATH
        this->_died = false;
#endif
    }

  // swap in and out states
    void swap ()
    {
        this->_inIdx ^= 1;
    }

#ifdef DIDEROT_HAS_START_METHOD
  // invoke strand's start method
    diderot::strand_status strand_start (@START_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        return @STRAND@_start (@START_ARGS@
            &self->_local,
            &self->_shared[this->_inIdx],
            &self->_shared[this->_inIdx^1]);
    }
#endif // DIDEROT_HAS_START_METHOD

  // invoke strand's update method
    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        return @STRAND@_update (@UPDATE_ARGS@
            &self->_local,
            &self->_shared[this->_inIdx],
            &self->_shared[this->_inIdx^1]);
    }

  // invoke strand's stabilize method
    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)
    {
        @STRANDTY@ *self = this->strand(ix);
        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];
        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];
#ifdef DIDEROT_HAS_STABILIZE_METHOD
      // note that we swap out and in here because out holds the current state
        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);
        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));
#else
        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));
#endif // DIDEROT_HAS_STABILIZE_METHOD
        this->_status[ix] = diderot::kStable;
        this->_nActive--;
        this->_nStable++;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // mark the given strand as dead
    index_t kill (index_t ix)
    {
#ifdef TRACK_STRAND_DEATH
        this->_died = true;
#endif
        this->_status[ix] = diderot::kDead;
        this->_nActive--;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // finish the local-phase of a superstep (NOP)
#ifdef TRACK_STRAND_DEATH
    bool finish_step ()
    {
        bool res = this->_died;
        this->_died = false;
        return res;
    }
#else
    bool finish_step () { return false; }
#endif

  // finish a kill_all operation (NOP)
    void finish_kill_all () { }

  // finish a stabilize_all operation (NOP)
    void finish_stabilize_all () { }

    index_t begin_alive () const
    {
        index_t ix = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }
    index_t end_alive () const { return this->_nItems; }
    index_t next_alive (index_t &ix) const
    {
        ix++;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }

  // iterator over active strands
    index_t begin_active () const
    {
        index_t ix = 0;
        while ((ix < this->_nItems) && notActiveSts(this->status(ix))) {
            ix++;
        }
        return ix;
    }
    index_t end_active () const { return this->_nItems; }
    index_t next_active (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over stable strands
    index_t begin_stable () const
    {
        index_t ix = 0;
        while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable)) {
            ix++;
        }
        return ix;
    }
    index_t end_stable () const { return this->_nItems; }
    index_t next_stable (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable));
        return ix;
    }

  // iterator over fresh strands; since the only new strands were created by create_strand
  // we iterate over all of them
    index_t begin_fresh () const { return 0; }
    index_t end_fresh () const { return this->_nFresh; }
    index_t next_fresh (index_t &ix) const { return ++ix; }

}; // struct strand_array

strand_array::~strand_array ()
{
  // run destructors to reclaim any dynamic memory attached to the strand state
    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {
        this->strand(ix)->~@STRANDTY@();
    }
    if (this->_status != nullptr) std::free (this->_status);
    if (this->_storage != nullptr) std::free (this->_storage);
}
/*---------- end seq-sarr-dual.in ----------*/
  4/*---------- begin seq-sarr-indirect.in ----------*/
// forward declarations of strand methods
#ifdef DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_start (@START_PARAMS@@STRANDTY@ *self);
#endif // DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@@STRANDTY@ *self);
#ifdef DIDEROT_HAS_STABILIZE_METHOD
static void @STRAND@_stabilize (@STABILIZE_PARAMS@@STRANDTY@ *self);
#endif // DIDEROT_HAS_STABILIZE_METHOD

// strand_array for BSP/SINGLE STATE/INDIRECT ACCESS
//
struct strand_array {
    typedef @STRANDTY@ strand_t;
    typedef uint32_t index_t;           // strand index (index into _idx and _status arrays)
    typedef uint32_t sid_t;             // strand ID (index into strand-state storage)
    typedef char *block_t;              // points to array of @STRANDTY@ structs

    uint8_t             *_status;       // the array of status information for the strands
    uint32_t            *_idx;          // array of strand indices for indirect state rep.
    std::vector<block_t> _blocks;       // vector of pointers to strand-storage blocks
    uint32_t            _nItems;        // number of items in the _blocks and _status arrays
    uint32_t            _nStable;       // stable strands (in locations 0.._nStable-1)
    uint32_t            _nActive;       // active strands (in locations _nStable.._nStable+_nActive-1)
    uint32_t            _nStabilizing;  // number of stablizing strands
    uint32_t            _nDying;        // number of dying strands
    uint32_t            _nNew;          // number of new strands
    uint32_t            _nFresh;        // number of fresh strands (new strands from previous step)

  // size info for block_t objects
    static const uint32_t LOG_BLKSZ = 12;               // 2^12 items per block
    static const uint32_t BLKSZ = (1 << LOG_BLKSZ);
    static const uint32_t BLKMASK = (BLKSZ-1);          // mask for block index

    strand_array () : _status(nullptr), _idx(nullptr), _nItems(0) { }
    ~strand_array ();

    uint32_t in_state_index () const { return 0; /* dummy */ }

    uint32_t num_active () const { return this->_nActive; }
    uint32_t num_stable () const { return this->_nStable; }
    uint32_t num_alive () const { return this->_nActive+this->_nStable; }
    uint32_t num_fresh () const { return this->_nFresh; }

  // return the ID of a strand, which is the value of the _idx array
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nItems);
        return this->_idx[ix];
    }
  // direct indexing of strands by ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        assert (id < this->_nItems);
        uint32_t blkId = id >> LOG_BLKSZ;
        uint32_t offset = id & BLKMASK;
        return reinterpret_cast<@STRANDTY@ *>(this->_blocks[blkId] + offset * sizeof(@STRANDTY@));
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        assert (ix < this->_nItems);
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }
  // return a pointer to the local state of strand ix
    @STRANDTY@ *local_state (index_t ix) const
    {
        return this->strand(ix);
    }
  // return a pointer to the local state of strand with the given ID
    @STRANDTY@ *id_to_local_state (sid_t id) const
    {
        return this->id_to_strand(id);
    }

  // wrappers for accessing the state of newly created strands
    @STRANDTY@ *new_strand_state (index_t ix) const
    {
        return this->strand(ix);
    }

  // is an index valid for the strand array?
    bool validIndex (index_t ix) const { return (ix < this->_nItems); }

  // is a given strand alive?
    bool isAlive (index_t ix) const
    {
#ifdef DIDEROT_HAS_STRAND_DIE
        return aliveSts(this->status(ix));
#else
        return true;
#endif
    }

  // deallocate space reserved for strands
    void dealloc ();

  // allocate space for at least nItems
    bool alloc (uint32_t nItems)
    {
        nItems = (nItems + BLKSZ - 1) & ~BLKMASK;
        uint32_t nBlks = nItems >> LOG_BLKSZ;
        assert (nItems == nBlks*BLKSZ);
      // allocate block vector
        this->_blocks.resize(nBlks, nullptr);
      // allocate blocks
        for (int i = 0;  i < nBlks;  i++) {
            this->_blocks[i] = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));
            if (this->_blocks[i]  == nullptr) {
              // unable to allocate memory
                this->dealloc();
                return true;
            }
        }
      // allocate _status array
        this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));
        if (this->_status == nullptr) {
            this->dealloc();
            return true;
        }
      // allocate _idx array
        this->_idx = static_cast<uint32_t *>(std::malloc (nItems * sizeof(uint32_t)));
        if (this->_idx == nullptr) {
            this->dealloc();
            return true;
        }
      // initialize arrays
        for (index_t ix = 0;  ix < nItems;  ix++) {
            this->_status[ix] = diderot::kDead;
            this->_idx[ix] = ix;
        }
        this->_nItems = nItems;
        this->_nActive = 0;
        this->_nStable = 0;
        this->_nStabilizing = 0;
        this->_nNew = 0;
        this->_nDying = 0;
        this->_nFresh = 0;
        return false;
    }

  // initialize the first nStrands locations as new active strands
    void create_strands (uint32_t nStrands)
    {
        assert (this->_nActive == 0);
        assert (this->_nItems >= nStrands);
        for (index_t ix = 0;  ix < nStrands;  ix++) {
            this->_status[ix] = diderot::kActive;
            new (this->strand(ix)) @STRANDTY@;
        }
        this->_nActive = nStrands;
        this->_nFresh = nStrands;
    }

  // swap in and out states (NOP for this version)
    void swap () { }

#ifdef DIDEROT_HAS_START_METHOD
  // invoke strand's start method
    diderot::strand_status strand_start (@START_PARAMS@index_t ix)
    {
        return @STRAND@_start(@START_ARGS@this->strand(ix));
    }
#endif // DIDEROT_HAS_START_METHOD

  // invoke strand's update method
    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)
    {
        return @STRAND@_update(@UPDATE_ARGS@this->strand(ix));
    }

  // invoke strand's stabilize method
    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)
    {
#ifdef DIDEROT_HAS_STABILIZE_METHOD
        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));
#endif // DIDEROT_HAS_STABILIZE_METHOD
      // we swap the strand-indices at ix and _nStable + this->_nStabilizing
        uint32_t jx = this->_nStable + this->_nStabilizing;
        this->_status[jx] = diderot::kStabilize;
        std::swap (this->_idx[ix], this->_idx[jx]);
        this->_nStabilizing++;
        return ix+1;
    }

  // record that the specified strand is dying
    index_t kill (index_t ix)
    {
        assert (this->_nStable <= ix);
        assert (ix < this->num_alive());
        this->_nDying++;
        uint32_t jx = this->num_alive() - this->_nDying;
        this->_status[jx] = diderot::kDie;
        std::swap (this->_idx[ix], this->_idx[jx]);
        return ix;  // don't advance, since ix is an active strand after the swap
    }

  // allocate a new strand
    index_t new_strand ()
    {
        index_t ix = this->num_alive() + this->_nNew;
        if (this->_nItems <= ix) {
            if (this->grow ()) {
                std::cerr << "Fatal error: unable to allocate space for new strands" << std::endl;
                exit (1);
            }
        }
        this->_status[ix] = diderot::kNew;
        new (this->strand(ix)) @STRANDTY@;
        this->_nNew++;
        return ix;
    }

  // finish the local-phase of a superstep by updating the strand statuses and
  // the various counters.  Return true if there were any births/deaths
    bool finish_step ()
    {
        bool anyNewDie = ((this->_nDying + this->_nNew) > 0);
        index_t next = this->_nStable;
        for (index_t ix = 0;  ix < this->_nStabilizing;  ix++, next++) {
            this->_status[next] = diderot::kStable;
        }
        if (this->_nDying == 0) {
          // no need to swap strands
            index_t next = this->num_alive();
            for (auto ix = 0;  ix < this->_nNew;  ix++, next++) {
                this->_status[next] = diderot::kActive;
            }
        }
        else {
          // first handle the dying
            next = this->num_alive() - this->_nDying;
            for (index_t ix = 0;  ix < this->_nDying;  ix++, next++) {
                this->_status[next] = diderot::kDead;
              // invoke the dead strand's destructors
                reinterpret_cast<@STRANDTY@ *>(this->strand(next))->~@STRANDTY@();
            }
          // move the new strands down over the dying strands
            index_t src = this->num_alive();
            index_t dst = src - this->_nDying;
            for (auto ix = 0;  ix < this->_nNew;  ix++, dst++, src++) {
                this->_status[dst] = diderot::kActive;
                this->_status[src] = diderot::kDead;
                std::swap (this->_idx[src], this->_idx[dst]);
            }
        }

      // update counts
        this->_nFresh = this->_nNew;
        this->_nStable += this->_nStabilizing;
        this->_nActive -= this->_nStabilizing + this->_nDying;
        this->_nActive += this->_nNew;
        this->_nStabilizing = 0;
        this->_nNew = 0;
        this->_nDying = 0;

        return anyNewDie;
    }

  // finish a kill_all operation
    void finish_kill_all ()
    {
        this->_nActive -= this->_nDying;
        this->_nDying = 0;
    }

  // finish a stabilize_all operation
    void finish_stabilize_all ()
    {
        this->_nStable += this->_nStabilizing;
        this->_nActive -= this->_nStabilizing;
        this->_nStabilizing = 0;
    }

  // iterator over stable strands
    index_t begin_stable () const { return 0; }
    index_t end_stable () const { return this->_nStable; }
    index_t next_stable (index_t &ix) const { return ++ix; }

  // iterator over active strands
    index_t begin_active () const { return this->_nStable+this->_nStabilizing; }
    index_t end_active () const { return this->_nStable+this->_nActive-this->_nDying; }
    index_t next_active (index_t &ix) const { return ++ix; }

  // iterator over alive (active+stable) strands; we assume that _nStabilizing and _nNew are 0
    index_t begin_alive () const { return 0; }
    index_t end_alive () const { return this->num_alive(); }
    index_t next_alive (index_t &ix) const { return ++ix; }

  // iterator over fresh strands
    index_t begin_fresh () const { return this->num_alive() - this->_nFresh; }
    index_t end_fresh () const { return this->num_alive(); }
    index_t next_fresh (index_t &ix) const { return ++ix; }

  // allocate more space for strand state; return true on error
    bool grow ()
    {
        size_t nItems = static_cast<size_t>(this->_nItems) + BLKSZ;
        if (nItems >= UINT32_MAX) {
          // cannot have more than UINT32_MAX elements
            return true;
        }

      // allocate a new block at the end of the _blocks array
        char *blk = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));
        if (blk == nullptr) {
            return true;
        }
        this->_blocks.push_back (blk);

      // grow the _status and _idx arrays
        uint8_t *status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));
        uint32_t *idx = static_cast<uint32_t *>(std::malloc (nItems * sizeof(uint32_t)));
        if ((status == nullptr) || (idx == nullptr)) {
            return true;
        }
        std::memcpy (status, this->_status, this->_nItems * sizeof(uint8_t));
        std::memcpy (idx, this->_idx, this->_nItems * sizeof(uint32_t));

      // initialize the new storage
        @STRANDTY@ *p = reinterpret_cast<@STRANDTY@ *>(blk);
        for (uint32_t ix = this->_nItems;  ix < nItems;  ix++) {
            status[ix] = diderot::kDead;
            idx[ix] = ix;
        }

      // free the old storage
        std::free (this->_status);
        std::free (this->_idx);

      // update pointers
        this->_status = status;
        this->_idx = idx;
        this->_nItems = nItems;

        return false;
    }

}; // struct strand_array

strand_array::~strand_array ()
{
  // run destructors to reclaim any dynamic memory attached to the strand state
    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {
        this->strand(ix)->~@STRANDTY@();
    }
    this->dealloc();
}

void strand_array::dealloc ()
{
    if (this->_status != nullptr) {
        std::free (this->_status);
        this->_status = nullptr;
    }
    if (this->_idx != nullptr) {
        std::free (this->_idx);
        this->_idx = nullptr;
    }
    for (uint32_t i = 0;  i < this->_blocks.size();  i++) {
        if (this->_blocks[i] != nullptr) {
            std::free (this->_blocks[i]);
            this->_blocks[i] = nullptr;
        }
        else {
            break;
        }
    }
}
/*---------- end seq-sarr-indirect.in ----------*/
   �/*---------- begin seq-sarr.in ----------*/
// forward declarations of strand methods
#ifdef DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_start (@START_PARAMS@@STRANDTY@ *self);
#endif // DIDEROT_HAS_START_METHOD
static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@@STRANDTY@ *self);
#ifdef DIDEROT_HAS_STABILIZE_METHOD
static void @STRAND@_stabilize (@STABILIZE_PARAMS@@STRANDTY@ *self);
#endif // DIDEROT_HAS_STABILIZE_METHOD

// if we have both communication and "die", then we need to track when strands die
// so that we can rebuild the list of strands use to construct the kd-tree
#if defined(DIDEROT_HAS_STRAND_COMMUNICATION) && !defined(DIDEROT_HAS_STRAND_DIE)
#  define TRACK_STRAND_DEATH
#endif

// strand_array for SEQUENTIAL/NO BSP/SINGLE STATE/DIRECT ACCESS
//
struct strand_array {
    typedef @STRANDTY@ strand_t;
    typedef uint32_t index_t;
    typedef index_t sid_t;              // strand ID (index into strand-state storage)

    uint8_t             *_status;       // the array of status information for the strands
    char                *_storage;      // points to array of @STRANDTY@ structs
    uint32_t            _nItems;        // number of items in the _storage and _status arrays
    uint32_t            _nStable;       // number of stable strands
    uint32_t            _nActive;       // number of active strands
    uint32_t            _nFresh;        // number of fresh strands (new strands from create_strands)
#ifdef TRACK_STRAND_DEATH
    bool                _died;          // a strand died in the current superstep.
#endif

    strand_array () : _status(nullptr), _storage(nullptr), _nItems(0) { }
    ~strand_array ();

    uint32_t in_state_index () const { return 0; /* dummy */ }

    uint32_t num_active () const { return this->_nActive; }
    uint32_t num_stable () const { return this->_nStable; }
    uint32_t num_alive () const { return this->_nActive+this->_nStable; }

  // return the ID of a strand, which is the same as the ix index
    sid_t id (index_t ix) const
    {
        assert (ix < this->_nItems);
        return ix;
    }
  // return a pointer to the strand with the given ID
    @STRANDTY@ *id_to_strand (sid_t id) const
    {
        assert (id < this->_nItems);
        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));
    }

  // return a strand's status
    diderot::strand_status status (index_t ix) const
    {
        assert (ix < this->_nItems);
        return static_cast<diderot::strand_status>(this->_status[ix]);
    }
  // return a pointer to the given strand
    @STRANDTY@ *strand (index_t ix) const
    {
        return this->id_to_strand(this->id(ix));
    }
  // return a pointer to the local state of strand ix
    @STRANDTY@ *local_state (index_t ix) const
    {
        return this->strand(ix);
    }
  // return a pointer to the local state of strand with the given ID
    @STRANDTY@ *id_to_local_state (sid_t id) const
    {
        return this->id_to_strand(id);
    }

  // is an index valid for the strand array?
    bool validIndex (index_t ix) const { return (ix < this->_nItems); }

  // is a given strand alive?
    bool isAlive (index_t ix) const
    {
#ifdef DIDEROT_HAS_STRAND_DIE
        return aliveSts(this->status(ix));
#else
        return true;
#endif
    }

  // allocate space for nItems
    bool alloc (uint32_t nItems)
    {
        this->_storage = static_cast<char *>(std::malloc (nItems * sizeof(@STRANDTY@)));
        if (this->_storage == nullptr) {
            return true;
        }
        this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));
        if (this->_status == nullptr) {
            std::free (this->_storage);
            return true;
        }
        this->_nItems = nItems;
        this->_nActive = 0;
        this->_nStable = 0;
        this->_nFresh = 0;
        return false;
    }

  // initialize the first nStrands locations as new active strands
    void create_strands (uint32_t nStrands)
    {
        assert (this->_nActive == 0);
        assert (this->_nItems == nStrands);
        for (index_t ix = 0;  ix < nStrands;  ix++) {
            this->_status[ix] = diderot::kActive;
            new (this->strand(ix)) @STRANDTY@;
        }
        this->_nActive = nStrands;
        this->_nFresh = nStrands;
#ifdef TRACK_STRAND_DEATH
        this->_died = false;
#endif
    }

  // swap in and out states (NOP for this version)
    void swap () { }

#ifdef DIDEROT_HAS_START_METHOD
  // invoke strand's start method
    diderot::strand_status strand_start (@START_PARAMS@index_t ix)
    {
        return @STRAND@_start(@START_ARGS@this->strand(ix));
    }
#endif // DIDEROT_HAS_START_METHOD

  // invoke strand's update method
    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)
    {
        return @STRAND@_update(@UPDATE_ARGS@this->strand(ix));
    }

  // invoke strand's stabilize method
    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)
    {
#ifdef DIDEROT_HAS_STABILIZE_METHOD
        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));
#endif // DIDEROT_HAS_STABILIZE_METHOD
        this->_status[ix] = diderot::kStable;
        this->_nActive--;
        this->_nStable++;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // mark the given strand as dead
    index_t kill (index_t ix)
    {
#ifdef TRACK_STRAND_DEATH
        this->_died = true;
#endif
        this->_status[ix] = diderot::kDead;
        this->_nActive--;
      // skip to next active strand
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // finish the local-phase of a superstep (NOP)
#ifdef TRACK_STRAND_DEATH
    bool finish_step ()
    {
        bool res = this->_died;
        this->_died = false;
        return res;
    }
#else
    bool finish_step () { return false; }
#endif

  // finish a kill_all operation (NOP)
    void finish_kill_all () { }

  // finish a stabilize_all operation (NOP)
    void finish_stabilize_all () { }

    index_t begin_alive () const
    {
        index_t ix = 0;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }
    index_t end_alive () const { return this->_nItems; }
    index_t next_alive (index_t &ix) const
    {
        ix++;
#ifdef DIDEROT_HAS_STRAND_DIE
        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {
            ix++;
        }
#endif
        return ix;
    }

  // iterator over active strands
    index_t begin_active () const
    {
        index_t ix = 0;
        while ((ix < this->_nItems) && notActiveSts(this->status(ix))) {
            ix++;
        }
        return ix;
    }
    index_t end_active () const { return this->_nItems; }
    index_t next_active (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));
        return ix;
    }

  // iterator over stable strands
    index_t begin_stable () const
    {
        index_t ix = 0;
        while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable)) {
            ix++;
        }
        return ix;
    }
    index_t end_stable () const { return this->_nItems; }
    index_t next_stable (index_t &ix) const
    {
        do {
            ix++;
        } while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable));
        return ix;
    }

  // iterator over fresh strands; since the only new strands were created by create_strand
  // we iterate over all of them
    index_t begin_fresh () const { return 0; }
    index_t end_fresh () const { return this->_nFresh; }
    index_t next_fresh (index_t &ix) const { return ++ix; }

}; // struct strand_array

strand_array::~strand_array ()
{
  // run destructors to reclaim any dynamic memory attached to the strand state
    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {
        this->strand(ix)->~@STRANDTY@();
    }
    if (this->_status != nullptr) std::free (this->_status);
    if (this->_storage != nullptr) std::free (this->_storage);
}
/*---------- end seq-sarr.in ----------*/
  �/*---------- begin world-methods.in ----------*/
// Allocate the program's world
//
world::world ()
    : diderot::world_base (ProgramName, @IS_GRID@, @NUM_AXES@)
{
#ifndef DIDEROT_NO_GLOBALS
    this->_globals = new globals;
#endif

#ifdef DIDEROT_HAS_STRAND_COMMUNICATION
    this->_tree = nullptr;
#endif
} // world constructor

// shutdown and deallocate the world
//
world::~world ()
{
#ifndef DIDEROT_NO_GLOBALS
    delete this->_globals;
#endif

#ifdef DIDEROT_HAS_STRAND_COMMUNICATION
    delete this->_tree;
#endif

} // world destructor

// Initialize the program's world
//
bool world::init ()
{
    assert (this->_stage == diderot::POST_NEW);

#if !defined(DIDEROT_STANDALONE_EXEC) && !defined(DIDEROT_NO_INPUTS)
  // initialize the defined flags for the input globals
    init_defined_inputs (this);
#endif

#ifdef DIDEROT_TARGET_PARALLEL
  // get CPU info
    if (this->_sched->get_cpu_info (this)) {
        return true;
    }
#endif

    this->_stage = diderot::POST_INIT;

    return false;

}

// allocate the initial strands and initialize the rest of the world structure.
//
bool world::alloc (int32_t base[@NUM_AXES@], uint32_t size[@NUM_AXES@])
{
    size_t numStrands = 1;
    for (uint32_t i = 0;  i < @NUM_AXES@;  i++) {
        numStrands *= size[i];
        this->_base[i] = base[i];
        this->_size[i] = size[i];
    }

    if (this->_verbose) {
        std::cerr << "world::alloc: " << size[0];
        for (uint32_t i = 1;  i < @NUM_AXES@;  i++) {
            std::cerr << " x " << size[i];
        }
        std::cerr << std::endl;
    }

#ifdef DIDEROT_TARGET_PARALLEL
  // determine the block size based on the initial number of strands and the
  // number of workers
    this->_strands.set_block_size (this->_sched->_numWorkers, numStrands);
#endif

  // allocate the strand array
    if (this->_strands.alloc (numStrands)) {
        biffMsgAdd (this->_errors, "unable to allocate strand-state array\n");
        return true;
    }

  // initialize strand state pointers etc.
    this->_strands.create_strands (numStrands);

#ifdef DIDEROT_HAS_STRAND_COMMUNICATION
    this->_tree = new diderot::kdtree<@SPATIAL_DIM@, @REALTY@, strand_array> (&this->_strands);
#endif

    return false;

} // world::alloc

// swap input and output states
//
inline void world::swap_state ()
{
    this->_strands.swap ();
}

#ifdef DIDEROT_HAS_KILL_ALL
void world::kill_all ()
{
    if (this->_strands.num_active() > 0) {
        for (auto ix = this->_strands.begin_active();
            ix != this->_strands.end_active();
            )
        {
            assert (this->_strands.status(ix) == diderot::kActive);
            ix = this->_strands.kill (ix);
        }
        this->_strands.finish_kill_all();
    }
    assert (this->_strands.num_active() == 0);
}
#endif

#ifdef DIDEROT_HAS_STABILIZE_ALL
void world::stabilize_all ()
{
#ifndef DIDEROT_NO_GLOBALS
    globals *glob = this->_globals;
#endif

    if (this->_strands.num_active() > 0) {
        for (auto ix = this->_strands.begin_active();
            ix != this->_strands.end_active();
            )
        {
            assert (this->_strands.status(ix) == diderot::kActive);
            this->_strands._status[ix] = diderot::kStable;
            ix = this->_strands.strand_stabilize (@STABILIZE_ARGS_IN_WRLD@ix);
        }
        this->_strands.finish_stabilize_all();
    }
    assert (this->_strands.num_active() == 0);
}
#endif
/*---------- end world-methods.in ----------*/
	       	       	   �       �D$H� �D$;|$ws�E �D$P��  �E� �G�w�_�O�W�G  �M�O�U�W �]�_$�w�w(�L$P��G�U�M�\$H�\$P�\$L�ؽ   �t$���   ��0�d$H�T  ��x����D$;|$w"�)�l$H�L$L�   �   �t$���   �d$H�  ���@����D$;|$w'�
�L$H�T$L�   �   �   �t$���   �d$H��   ������D$;|$w)�+�u �S�K�[�t$H�l$L�   �D$��<  �d$H�   ���������D$;|$w"�)�l$H�L$L�   �   �t$��p  �d$H�V�������D$;|$w'��L$H�\$L�   �   �   �t$���  �d$H����;|$w�p�(�P�H�X��� �D$H   �D$L   �T$ ���T$ �d$Htarget-cpu/fragments.sml   1p�CPUFragments"5DCA nff9pa"cWrappers"4��nC"string"0 pa"parMain"4�(pa"parRun"4�(pa"parRunStartMethods"4�(Cpa"parWorkerNoBSP"4�(pa"parWorker"4�(pa"parSArrayDualInd"4�(pa"parSArrayDualDir"4�(pa"parSArrayInd"4�(Cpa"parSArrayDir"4�(	pa"seqMain"4�(
pa"seqRunNoBSP"4�(pa"seqRun"4�(pa"seqRunStartMethods"4�(Cpa"seqSArrayDualInd"4�(pa"seqSArrayDualDir"4�(pa"seqSArrayInd"4�(pa"seqSArrayDir"4�(pa"worldMethods"4�(N00sABi1�A 9�B��3��3��3C��3��3��3��3��3C��3��3��3��3��3C��3��3��3��3��3N