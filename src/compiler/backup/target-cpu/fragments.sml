(* fragments.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * !!! THIS FILE WAS GENERATED; DO NOT EDIT !!!
 *)

structure CPUFragments =
  struct

    val cWrappers = "\
          \/*---------- begin c-wrappers.in ----------*/\n\
          \extern \"C\" uint32_t @PREFIX@_num_strands (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    return w->_strands.num_alive();\n\
          \}\n\
          \\n\
          \extern \"C\" uint32_t @PREFIX@_num_active_strands (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    return w->_strands.num_active();\n\
          \}\n\
          \\n\
          \extern \"C\" uint32_t @PREFIX@_num_stable_strands (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    return w->_strands.num_stable();\n\
          \}\n\
          \\n\
          \extern \"C\" @BOOLTY@ @PREFIX@_any_errors (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    return (w->_errors->errNum > 0);\n\
          \}\n\
          \\n\
          \extern \"C\" char *@PREFIX@_get_errors (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    char *msg = biffMsgStrGet (w->_errors);\n\
          \    biffMsgClear (w->_errors);\n\
          \    return msg;\n\
          \}\n\
          \\n\
          \extern \"C\" @PREFIX@_world_t *@PREFIX@_new_world ()\n\
          \{\n\
          \    @PREFIX@::world *w = new (std::nothrow) @PREFIX@::world();\n\
          \    return reinterpret_cast<@PREFIX@_world_t *>(w);\n\
          \}\n\
          \\n\
          \extern \"C\" @BOOLTY@ @PREFIX@_init_world (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \\n\
          \    if (w->_stage != diderot::POST_NEW) {\n\
          \        w->error (\"multiple calls to @PREFIX@_init_world\");\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \    if (w->init()) {\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \#ifndef DIDEROT_NO_INPUTS\n\
          \    if (w != nullptr) {\n\
          \        init_defined_inputs (w);\n\
          \        init_defaults (w->_globals);\n\
          \    }\n\
          \#endif\n\
          \\n\
          \    return false;\n\
          \}\n\
          \\n\
          \extern \"C\" @BOOLTY@ @PREFIX@_create_strands (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \\n\
          \    if (w->_stage < diderot::POST_INIT) {\n\
          \        w->error (\"must call @PREFIX@_init_world before @PREFIX@_create_strands\");\n\
          \        return true;\n\
          \    }\n\
          \    else if (w->_stage > diderot::POST_INIT) {\n\
          \        w->error (\"multiple calls to @PREFIX@_create_strands\");\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \#ifndef DIDEROT_NO_INPUTS\n\
          \    if (check_defined(w)) {\n\
          \        return true;\n\
          \    }\n\
          \#endif\n\
          \    return static_cast<@BOOLTY@>(w->create_strands());\n\
          \}\n\
          \\n\
          \extern \"C\" uint32_t @PREFIX@_run (@PREFIX@_world_t *wrld, uint32_t maxNSteps)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \\n\
          \    if (w->_stage < diderot::POST_CREATE) {\n\
          \        w->error (\"attempt to run uninitialized program\");\n\
          \        return 0;\n\
          \    }\n\
          \    else if (w->_stage == diderot::DONE) {\n\
          \        return 0;\n\
          \    }\n\
          \\n\
          \    return w->run(maxNSteps);\n\
          \}\n\
          \\n\
          \extern \"C\" void @PREFIX@_shutdown (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    delete w;\n\
          \}\n\
          \\n\
          \extern \"C\" void @PREFIX@_set_verbose (@PREFIX@_world_t *wrld, @BOOLTY@ mode)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    w->_verbose = (mode ? true : false);\n\
          \}\n\
          \\n\
          \extern \"C\" @BOOLTY@ @PREFIX@_get_verbose (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    return static_cast<@BOOLTY@>(w->_verbose);\n\
          \}\n\
          \\n\
          \#ifdef DIDEROT_TARGET_PARALLEL\n\
          \\n\
          \bool @PREFIX@_set_num_workers (@PREFIX@_world_t *wrld, uint32_t nw)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    if (w->_sched->_numHWCores < nw) {\n\
          \        w->_sched->_numWorkers = w->_sched->_numHWCores;\n\
          \        return true;\n\
          \    }\n\
          \    else if (nw > 0) {\n\
          \        w->_sched->_numWorkers = nw;\n\
          \    }\n\
          \    else {\n\
          \        w->_sched->_numWorkers = w->_sched->_numHWCores;\n\
          \    }\n\
          \    return false;\n\
          \}\n\
          \\n\
          \uint32_t @PREFIX@_get_num_workers (@PREFIX@_world_t *wrld)\n\
          \{\n\
          \    @PREFIX@::world *w = reinterpret_cast<@PREFIX@::world *>(wrld);\n\
          \    return w->_sched->_numWorkers;\n\
          \}\n\
          \\n\
          \#endif /* DIDEROT_TARGET_PARALLEL */\n\
          \/*---------- end c-wrappers.in ----------*/\n\
          \"

    val parMain = "\
          \/*---------- begin par-main.in ----------*/\n\
          \using namespace @PREFIX@;\n\
          \\n\
          \//! Main function for standalone parallel C target\n\
          \//\n\
          \int main (int argc, const char **argv)\n\
          \{\n\
          \    bool        timingFlg = false;      //! true if timing computation\n\
          \    uint32_t    stepLimit = 0;          //! limit on number of execution steps (0 means unlimited)\n\
          \    std::string printFile = \"-\";        //! file to direct printed output into\n\
          \    uint32_t    reqNumWorkers;          //! requested number of worker threads\n\
          \#ifdef DIDEROT_EXEC_SNAPSHOT\n\
          \    uint32_t    snapshotPeriod = 1;     //! supersteps per snapshot\n\
          \#endif\n\
          \    uint32_t    nSteps = 0;             //! number of supersteps taken\n\
          \\n\
          \  // create the world\n\
          \    world *wrld = new (std::nothrow) world();\n\
          \    if (wrld == nullptr) {\n\
          \        std::cerr << \"unable to create world\" << std::endl;\n\
          \        exit (1);\n\
          \    }\n\
          \\n\
          \  // initialize scheduler stuff\n\
          \    if (wrld->_verbose) {\n\
          \        std::cerr << \"CPU info: \" << wrld->_sched->_numHWCores << \" cores / \"\n\
          \            << wrld->_sched->_numHWThreads << \" threads\\n\";\n\
          \        std::cerr << \"initializing world ...\" << std::endl;\n\
          \    }\n\
          \    if (wrld->init()) {\n\
          \        std::cerr << \"Error initializing world:\\n\" << wrld->get_errors() << std::endl;\n\
          \        delete wrld;\n\
          \        exit(1);\n\
          \    }\n\
          \\n\
          \#ifndef DIDEROT_NO_INPUTS\n\
          \  // initialize the default values for the inputs\n\
          \    cmd_line_inputs inputs;\n\
          \    init_defaults (&inputs);\n\
          \#endif\n\
          \\n\
          \  // handle command-line options\n\
          \    {\n\
          \        diderot::options *opts = new diderot::options ();\n\
          \        reqNumWorkers = wrld->_sched->_numHWCores;\n\
          \        opts->add (\"l,limit\", \"specify limit on number of super-steps (0 means unlimited)\",\n\
          \            &stepLimit, true);\n\
          \#ifdef DIDEROT_EXEC_SNAPSHOT\n\
          \        opts->add (\"s,snapshot\",\n\
          \            \"specify number of super-steps per snapshot (0 means no snapshots)\",\n\
          \            &snapshotPeriod, true);\n\
          \#endif\n\
          \        opts->add (\"print\", \"specify where to direct printed output\", &printFile, true);\n\
          \        opts->addFlag (\"v,verbose\", \"enable runtime-system messages\", &(wrld->_verbose));\n\
          \        opts->addFlag (\"t,timing\", \"enable execution timing\", &timingFlg);\n\
          \        opts->add (\"n,nworkers\", \"specify number of worker threads\", &reqNumWorkers, true);\n\
          \#ifndef DIDEROT_NO_INPUTS\n\
          \      // register options for setting global inputs\n\
          \        register_inputs (&inputs, opts);\n\
          \#endif\n\
          \        register_outputs (opts);\n\
          \        opts->process (argc, argv);\n\
          \        delete opts;\n\
          \    }\n\
          \\n\
          \  // redirect printing (if necessary)\n\
          \    if (printFile.compare(\"-\") != 0) {\n\
          \        wrld->_printTo = new std::ofstream (printFile);\n\
          \        if (wrld->_printTo->fail()) {\n\
          \            std::cerr << \"Error opening print file\" << std::endl;\n\
          \            delete wrld;\n\
          \            exit(1);\n\
          \        }\n\
          \    }\n\
          \\n\
          \    wrld->_sched->set_num_workers (reqNumWorkers);\n\
          \#ifdef DIDEROT_ENABLE_LOGGING\n\
          \  // initialize logging\n\
          \    wrld->_log_file = new diderot::log::file(\"@LOG_FILE@\", wrld->_sched);\n\
          \#endif\n\
          \    if (wrld->_sched->create_workers (wrld)) {\n\
          \        std::cerr << \"Error creating workers:\\n\" << wrld->get_errors() << std::endl;\n\
          \        delete wrld;\n\
          \        exit(1);\n\
          \    }\n\
          \\n\
          \#ifndef DIDEROT_NO_INPUTS\n\
          \  // initialize the input globals\n\
          \    if (init_inputs (wrld, &inputs)) {\n\
          \        std::cerr << \"Error initializing inputs:\\n\" << wrld->get_errors() << std::endl;\n\
          \        delete wrld;\n\
          \        exit(1);\n\
          \    }\n\
          \#endif\n\
          \\n\
          \  // run the generated global initialization code\n\
          \    if (wrld->_verbose) {\n\
          \        std::cerr << \"initializing globals and creating strands ...\\n\";\n\
          \    }\n\
          \    if (wrld->create_strands()) {\n\
          \        std::cerr << \"Error in global initialization:\\n\"\n\
          \            << wrld->get_errors() << std::endl;\n\
          \        delete wrld;\n\
          \        exit(1);\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_EXEC_SNAPSHOT\n\
          \\n\
          \    if (snapshotPeriod > 0) {\n\
          \     // write initial state as snapshot 0\n\
          \        write_snapshot (wrld, \"-0000\");\n\
          \     // run the program for `snapshotPeriod` steps at a time with a snapshot after each run\n\
          \        while (true) {\n\
          \            uint32_t n, limit;\n\
          \          // determine a step limit for the next run\n\
          \            if (stepLimit > 0) {\n\
          \                if (stepLimit <= nSteps) {\n\
          \                    break;\n\
          \                }\n\
          \                limit = std::min(stepLimit - nSteps, snapshotPeriod);\n\
          \            }\n\
          \            else {\n\
          \                limit = snapshotPeriod;\n\
          \            }\n\
          \          // run the program for upto limit steps\n\
          \            if ((n = wrld->run (limit)) == 0) {\n\
          \                break;\n\
          \            }\n\
          \            nSteps += n;\n\
          \            if ((wrld->_errors->errNum > 0) || (wrld->_strands.num_alive() == 0)) {\n\
          \                break;\n\
          \            }\n\
          \          // write a snapshot with the step count as a suffix\n\
          \            std::string suffix = std::to_string(nSteps);\n\
          \            if (suffix.length() < 4) {\n\
          \                suffix = std::string(\"0000\").substr(0, 4 - suffix.length()) + suffix;\n\
          \            }\n\
          \            suffix = \"-\" + suffix;\n\
          \            write_snapshot (wrld, suffix);\n\
          \        }\n\
          \    }\n\
          \    else {\n\
          \        nSteps = wrld->run (stepLimit);\n\
          \    }\n\
          \\n\
          \#else // !DIDEROT_EXEC_SNAPSHOT\n\
          \\n\
          \    nSteps = wrld->run (stepLimit);\n\
          \\n\
          \#endif // DIDEROT_EXEC_SNAPSHOT\n\
          \\n\
          \  // shutdown the workers\n\
          \    wrld->_sched->shutdown (wrld);\n\
          \\n\
          \    if (wrld->_errors->errNum > 0) {\n\
          \        std::cerr << \"Error during execution:\\n\" << wrld->get_errors() << std::endl;\n\
          \        delete wrld;\n\
          \        exit(1);\n\
          \    }\n\
          \\n\
          \    if ((stepLimit != 0) && (wrld->_strands.num_active() > 0)) {\n\
          \#ifdef DIDEROT_STRAND_ARRAY\n\
          \        if (wrld->_verbose) {\n\
          \            std::cerr << \"Step limit expired; \"\n\
          \                << wrld->_strands.num_active() << \" active strands remaining\" << std::endl;\n\
          \        }\n\
          \#else\n\
          \      // step limit expired, so kill remaining strands\n\
          \        if (wrld->_verbose) {\n\
          \            std::cerr << \"Step limit expired. Killing remaining \"\n\
          \                << wrld->_strands.num_active() << \" active strands\" << std::endl;\n\
          \        }\n\
          \        wrld->kill_all();\n\
          \#endif\n\
          \    }\n\
          \\n\
          \    if (wrld->_verbose) {\n\
          \        std::cerr << \"done: \" << nSteps << \" steps, in \" << wrld->_run_time << \" seconds\";\n\
          \#ifndef DIDEROT_STRAND_ARRAY\n\
          \        std::cerr << \"; \" << wrld->_strands.num_stable() << \" stable strands\" << std::endl;\n\
          \#else\n\
          \        std::cerr << std::endl;\n\
          \#endif\n\
          \    }\n\
          \    else if (timingFlg) {\n\
          \        std::cout << \"usr=\" << wrld->_run_time << std::endl;\n\
          \    }\n\
          \\n\
          \  // output the final strand states\n\
          \    write_output (wrld);\n\
          \\n\
          \    delete wrld;\n\
          \\n\
          \    return 0;\n\
          \\n\
          \} // main\n\
          \/*---------- end par-main.in ----------*/\n\
          \"

    val parRun = "\
          \/*---------- begin par-run.in ----------*/\n\
          \//! Run the Diderot program (parallel version)\n\
          \//! \\param maxNSteps the limit on the number of super steps; 0 means unlimited\n\
          \//! \\return the number of steps taken, or 0 on error.\n\
          \uint32_t world::run (uint32_t maxNSteps)\n\
          \{\n\
          \    if (this->_stage == diderot::POST_CREATE) {\n\
          \#ifdef DIDEROT_HAS_GLOBAL_START\n\
          \        this->global_start();\n\
          \#endif\n\
          \        this->_stage = diderot::RUNNING;\n\
          \    }\n\
          \    assert (this->_stage == diderot::RUNNING);\n\
          \\n\
          \    diderot::scheduler *sched = this->_sched;\n\
          \\n\
          \    if (maxNSteps == 0) {\n\
          \        maxNSteps = 0xffffffff;  // essentially unlimited\n\
          \    }\n\
          \\n\
          \  // set task pointer\n\
          \    sched->_task = worker;\n\
          \\n\
          \  // initialize per-worker info\n\
          \    this->_strands._workers.clear();\n\
          \    worker_arg *args = new worker_arg[sched->_numWorkers];\n\
          \    for (int i = 0;  i < sched->_numWorkers;  i++) {\n\
          \        worker_arg *p = &args[i];\n\
          \        p->_wrld = this;\n\
          \        p->_id = i;\n\
          \        p->_maxNSteps = maxNSteps;\n\
          \        p->_nSteps = 0;\n\
          \#ifndef DIDEROT_BSP\n\
          \        p->_nStable = 0;\n\
          \        p->_nDead = 0;\n\
          \#endif\n\
          \        p->_strands.init (this->_strands);\n\
          \        sched->_info[i]._data = p;\n\
          \    }\n\
          \\n\
          \    double t0 = airTime();\n\
          \\n\
          \  // Start worker threads\n\
          \    if (this->_verbose) {\n\
          \        std::cerr << \"run with \" << this->_strands.num_active() << \" active strands / \"\n\
          \            << sched->_numWorkers << \" workers ...\" << std::endl;\n\
          \    }\n\
          \    this->_strands.prepare_run ();\n\
          \    sched->_gate.release_workers (IF_LOGGING( this ));\n\
          \\n\
          \  // wait for the computation to finish\n\
          \    sched->_gate.controller_wait (IF_LOGGING( this ));\n\
          \\n\
          \  // get max # steps and update global counts of active and stable strands when no-bsp\n\
          \    uint32_t nSteps = 0;\n\
          \    for (uint32_t i = 0;  i < sched->_numWorkers;  i++) {\n\
          \        nSteps = std::max (nSteps, args[i]._nSteps);\n\
          \#ifndef DIDEROT_BSP\n\
          \      // if there is no BSP, then the controller updates #active and #stable\n\
          \        this->_strands._nActive -= args[i]._nStable + args[i]._nDead;\n\
          \        this->_strands._nStable += args[i]._nStable;\n\
          \#endif\n\
          \    }\n\
          \    delete[] args;\n\
          \\n\
          \    t0 = airTime() - t0;\n\
          \    if (this->_verbose) {\n\
          \        std::cerr << nSteps << \" steps done in \" << t0 << \" seconds\" << std::endl;\n\
          \    }\n\
          \    this->_run_time += t0;\n\
          \\n\
          \    return nSteps;\n\
          \\n\
          \} // world::run\n\
          \/*---------- end par-run.in ----------*/\n\
          \"

    val parRunStartMethods = "\
          \/*---------- begin par-run-start.in ----------*/\n\
          \// Run the start methods of the initial strands (parallel version)\n\
          \//\n\
          \void worker_cache::run_start_methods (@START_PARAMS@sched_block *bp)\n\
          \{\n\
          \    for (auto ix = this->begin_fresh(bp); ix != this->end_fresh(bp); )\n\
          \    {\n\
          \        diderot::strand_status sts = this->strand_start(@START_ARGS@ix);\n\
          \        switch (sts) {\n\
          \          case diderot::kStabilize:\n\
          \            ix = this->strand_stabilize (bp, @STABILIZE_ARGS@ix);\n\
          \            break;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \          case diderot::kDie:\n\
          \            ix = this->kill (bp, ix);\n\
          \            break;\n\
          \#endif\n\
          \          default:\n\
          \\t    this->_status[ix] = diderot::kActive;\n\
          \            ix = this->next_fresh(bp, ix);\n\
          \            break;\n\
          \        }\n\
          \    }\n\
          \\n\
          \}\n\
          \/*---------- end par-run-start.in ----------*/\n\
          \"

    val parWorkerNoBSP = "\
          \/*---------- begin par-worker-nobsp.in ----------*/\n\
          \struct CACHE_ALIGN worker_arg {\n\
          \    world       *_wrld;         //!< world pointer\n\
          \    uint32_t    _id;            //!< worker ID\n\
          \    uint32_t    _maxNSteps;     //!< maximum number of steps to take; 0 == infinity\n\
          \    uint32_t    _nSteps;        //!< max number of steps taken by a strand in call to run\n\
          \    uint32_t    _nStable;       //!< number of strands that stabilized in call to run\n\
          \    uint32_t    _nDead;         //!< number of strands that died in call to run\n\
          \    worker_cache _strands;\n\
          \};\n\
          \\n\
          \/* Worker task for when we do not need super-step synchronization */\n\
          \static void worker (void *arg)\n\
          \{\n\
          \    worker_arg *myArg = reinterpret_cast<worker_arg *>(arg);\n\
          \    world *wrld = myArg->_wrld;\n\
          \#ifndef DIDEROT_NO_GLOBALS\n\
          \    globals *glob = wrld->_globals;\n\
          \#endif\n\
          \\n\
          \  // iterate until there is no more work to do\n\
          \    uint32_t numDead = 0;\n\
          \    uint32_t numStabilized = 0;\n\
          \    uint32_t maxSteps = 0;\n\
          \    uint32_t maxNSteps = myArg->_maxNSteps;\n\
          \    strand_array::sched_block *blk;\n\
          \    IF_LOGGING ( LogGetStrandBlock(wrld, myArg->_id+1); )\n\
          \    while ((blk = myArg->_strands.get_block()) != nullptr) {\n\
          \        IF_LOGGING ( LogGotStrandBlock(wrld, myArg->_id+1); )\n\
          \        uint32_t nStable = blk->_nStable;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        uint32_t nDead = blk->_nDead;\n\
          \#endif\n\
          \      // update the strands\n\
          \        for (auto ix = myArg->_strands.begin_active(blk);\n\
          \            ix != myArg->_strands.end_active(blk);\n\
          \        ) {\n\
          \          // run the strand to completion, or until the step limit is exceeded\n\
          \            @STRANDTY@ *self = myArg->_strands.strand(ix);\n\
          \            diderot::strand_status sts = myArg->_strands.status(ix);\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \            if (sts == diderot::kNew) {\n\
          \                IF_LOGGING ( LogStrandStart(wrld, myArg->_id+1, ix); )\n\
          \                sts = @STRAND@_start(@START_ARGS@self);\n\
          \            }\n\
          \#endif\n\
          \            uint32_t nSteps = 0;\n\
          \            while ((! sts) && (nSteps < maxNSteps)) {\n\
          \                nSteps++;\n\
          \                sts = @STRAND@_update(@UPDATE_ARGS@self);\n\
          \            }\n\
          \            switch (sts) {\n\
          \              case diderot::kStabilize:\n\
          \              // stabilize the strand's state.\n\
          \                IF_LOGGING ( LogStrandStabilize(wrld, myArg->_id+1, ix); )\n\
          \                ix = myArg->_strands.strand_stabilize (blk, @STABILIZE_ARGS@ix);\n\
          \                break;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \              case diderot::kDie:\n\
          \                IF_LOGGING ( LogStrandDie(wrld, myArg->_id+1, ix); )\n\
          \                ix = myArg->_strands.kill (blk, ix);\n\
          \                break;\n\
          \#endif\n\
          \              default:\n\
          \                assert (sts == myArg->_strands.status(ix));\n\
          \                ix = myArg->_strands.next_active(blk, ix);\n\
          \                break;\n\
          \            }\n\
          \            if (maxSteps < nSteps) maxSteps = nSteps;\n\
          \        }\n\
          \        numStabilized += (blk->_nStable - nStable);\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        numDead += (blk->_nDead - nDead);\n\
          \#endif\n\
          \        IF_LOGGING ( LogGetStrandBlock(wrld, myArg->_id+1); )\n\
          \    }\n\
          \    IF_LOGGING ( LogNoStrandBlock(wrld, myArg->_id+1); )\n\
          \\n\
          \  // update global counts of active and stable strands\n\
          \    myArg->_nSteps = maxSteps;\n\
          \    myArg->_nStable = numStabilized;\n\
          \    myArg->_nDead = numDead;\n\
          \\n\
          \}\n\
          \/*---------- end par-worker-nobsp.in ----------*/\n\
          \"

    val parWorker = "\
          \/*---------- begin par-worker.in ----------*/\n\
          \struct CACHE_ALIGN worker_arg {\n\
          \    world       *_wrld;         //!< world pointer\n\
          \    uint32_t    _id;            //!< worker ID\n\
          \    uint32_t    _maxNSteps;     //!< maximum number of steps to take; 0 == infinity\n\
          \    uint32_t    _nSteps;        //!< max number of steps taken by a strand in call to run\n\
          \    worker_cache _strands;\n\
          \};\n\
          \\n\
          \/* Function which processes active strands. */\n\
          \static void worker (void *arg)\n\
          \{\n\
          \    worker_arg *myArg = reinterpret_cast<worker_arg *>(arg);\n\
          \    world *wrld = myArg->_wrld;\n\
          \#ifndef DIDEROT_NO_GLOBALS\n\
          \    globals *glob = wrld->_globals;\n\
          \#endif\n\
          \    diderot::scheduler *sched = wrld->_sched;\n\
          \    worker_cache *strands = &(myArg->_strands);\n\
          \    bool treeNeedsUpdate = true;\n\
          \\n\
          \  // barrier before start of first super-step\n\
          \    sched->_bspBar.all_wait ();\n\
          \\n\
          \  // iterate until all strands are stable\n\
          \    uint32_t nSteps = 0;\n\
          \    uint32_t maxNSteps = myArg->_maxNSteps;\n\
          \\n\
          \    while ((wrld->_strands.num_active() > 0) && (nSteps < maxNSteps)) {\n\
          \        uint32_t numDead = 0;\n\
          \        uint32_t numStabilized = 0;\n\
          \        strands->refresh();\n\
          \        nSteps++;\n\
          \#ifdef DIDEROT_HAS_STRAND_COMMUNICATION\n\
          \      // build spatial partition to support communication\n\
          \        if (sched->_bspBar.wait(myArg->_id == 0)) {\n\
          \          // worker 0 does sequential work of rebuilding tree\n\
          \/* FIXME: tree building should be parallel */\n\
          \            IF_LOGGING ( LogKDTreeRebuildStart(wrld, myArg->_id+1); )\n\
          \            if (treeNeedsUpdate) {\n\
          \                wrld->_tree->update_strands ();\n\
          \            }\n\
          \            wrld->_tree->rebuild ();\n\
          \            IF_LOGGING ( LogKDTreeRebuildDone(wrld, myArg->_id+1); )\n\
          \          // synchronize on the tree having been built\n\
          \            sched->_bspBar.release();\n\
          \        }\n\
          \#endif\n\
          \        strand_array::sched_block *blk;\n\
          \        while ((blk = strands->get_block()) != nullptr) {\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \          // run start methods for fresh strands\n\
          \            strands->run_start_methods(@START_ARGS@blk);\n\
          \#endif\n\
          \            uint32_t nStable = blk->_nStable;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \            uint32_t nDead = blk->_nDead;\n\
          \#endif\n\
          \          // update the strands\n\
          \            for (auto ix = strands->begin_active(blk);\n\
          \                ix != strands->end_active(blk);\n\
          \            ) {\n\
          \                diderot::strand_status sts = strands->strand_update(@UPDATE_ARGS@ix);\n\
          \                switch (sts) {\n\
          \                  case diderot::kStabilize:\n\
          \                  // stabilize the strand's state.\n\
          \                    IF_LOGGING ( LogStrandStabilize(wrld, myArg->_id+1, ix); )\n\
          \                    ix = strands->strand_stabilize(blk, @STABILIZE_ARGS@ix);\n\
          \                    break;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \                  case diderot::kDie:\n\
          \                    IF_LOGGING ( LogStrandDie(wrld, myArg->_id+1, ix); )\n\
          \                    ix = strands->kill (blk, ix);\n\
          \                    break;\n\
          \#endif\n\
          \                  default:\n\
          \                    assert (sts == strands->status(ix));\n\
          \                    ix = strands->next_active(blk, ix);\n\
          \                    break;\n\
          \                }\n\
          \            }\n\
          \          // finish the local-phase of the superstep by updating strand status\n\
          \            numStabilized += (blk->_nStable - nStable);\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \            numDead += (blk->_nDead - nDead);\n\
          \#endif\n\
          \        }\n\
          \\n\
          \        strands->_nStabilizing = numStabilized;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        strands->_nDying = numDead;\n\
          \#endif\n\
          \      // barrier at end of local update phase\n\
          \        if (sched->_bspBar.wait (myArg->_id == 0)) {\n\
          \          // finish the local-phase of the superstep by updating strand status\n\
          \            treeNeedsUpdate = wrld->_strands.finish_step();\n\
          \            wrld->swap_state();\n\
          \#ifdef DIDEROT_HAS_GLOBAL_UPDATE\n\
          \/* FIXME: global update should be parallel */\n\
          \          // worker 0 does sequential work of global update\n\
          \            wrld->global_update();\n\
          \#endif\n\
          \            sched->_bspBar.release();\n\
          \        }\n\
          \        strands->swap();\n\
          \    }\n\
          \\n\
          \  // return number of steps\n\
          \    myArg->_nSteps = nSteps;\n\
          \\n\
          \}\n\
          \/*---------- end par-worker.in ----------*/\n\
          \"

    val parSArrayDualInd = "\
          \/*---------- begin par-sarr-dual-indirect.in ----------*/\n\
          \// forward declaration of worker_cache type\n\
          \struct worker_cache;\n\
          \// forward declarations of strand methods\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_start (@START_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \static void @STRAND@_stabilize (@STABILIZE_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \\n\
          \// strand_array for PARALLEL_TARGET/BSP/DUAL STATE/INDIRECT ACCESS\n\
          \//\n\
          \struct strand_array {\n\
          \    typedef @STRANDTY@ strand_t;\n\
          \    typedef uint32_t index_t;           // strand index (index into _idx and _status arrays)\n\
          \    typedef strand_t *sid_t;            // strand ID (pointer to strand-state storage)\n\
          \    typedef char *block_t;              // points to array of @STRANDTY@ structs\n\
          \\n\
          \    // scheduling block of strands\n\
          \    //\n\
          \    struct CACHE_ALIGN sched_block {\n\
          \        index_t         _start;         // first index in block\n\
          \        index_t         _stop;          // last index in block + 1\n\
          \        uint32_t        _nStable;       // number of stable strands in the block\n\
          \        uint32_t        _nDead;         // number of dead strands in the block; this will\n\
          \                                        // be equal to the block size for unused blocks\n\
          \      // we organize the strands in a sched_block so that the stable strands are at\n\
          \      // the beginning, followed by the active strands, followed by the dead strands.\n\
          \      // An unused block will have _nDead == num_strands()\n\
          \\n\
          \      // return the number of strands in the block\n\
          \        uint32_t num_strands () const { return this->_stop - this->_start; }\n\
          \      // return the number of alive strands in the block\n\
          \        uint32_t num_alive () const\n\
          \        {\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \            return this->num_strands() - this->_nDead;\n\
          \#else\n\
          \            return this->num_strands();\n\
          \#endif\n\
          \        }\n\
          \      // return the number of active strands in the block\n\
          \        uint32_t num_active () const\n\
          \        {\n\
          \            return this->num_alive() - this->_nStable;\n\
          \        }\n\
          \      // return index of next available slot in block (_stop if none)\n\
          \        index_t next_avail () const { return this->_stop - this->_nDead; }\n\
          \\n\
          \      // is the block being used?\n\
          \        bool in_use () const { return this->_nDead == this->num_strands(); }\n\
          \    };\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    sid_t               *_idx;          // array of strand indices for indirect state rep.\n\
          \    std::vector<block_t> _blocks;       // vector of pointers to strand-storage blocks\n\
          \    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling\n\
          \    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)\n\
          \    uint32_t            _arraySz;       // the number of allocated items in _status and _idx arrays\n\
          \    uint32_t            _nStrands;      // number of strands in the _status and _idx arrays\n\
          \                                        // (including dead strands)\n\
          \    uint32_t            _nSchedBlks;    // number of scheduling blocks in use\n\
          \    uint32_t            _nSchedBlksAlloc; // number of allocated scheduling blocks\n\
          \                                        // INV: _arraySz == _nSchedBlksAlloc * _schedBlkSz.\n\
          \    uint32_t            _schedBlkSz;    // size of scheduling blocks\n\
          \    atomic_uint32_t     _nextSchedBlk CACHE_ALIGN;\n\
          \                                        // next block to schedule\n\
          \    uint32_t            _nActive;       // global number of active strands\n\
          \    uint32_t            _nStable;       // global number of stable strands\n\
          \    pthread_mutex_t     _lock;          // lock for managing access to _blocks vector\n\
          \    std::vector<worker_cache *> _workers;\n\
          \\n\
          \  // size info for block_t objects\n\
          \    static const uint32_t LOG_BLKSZ = 12;               // 2^12 items per block\n\
          \    static const uint32_t BLKSZ = (1 << LOG_BLKSZ);\n\
          \    static const uint32_t BLKMASK = (BLKSZ-1);          // mask for block index\n\
          \\n\
          \    strand_array ()\n\
          \      : _status(nullptr), _idx(nullptr), _blocks(), _schedBlks(nullptr), _inIdx(0),\n\
          \        _arraySz(0), _nStrands(0), _nSchedBlks(0), _nSchedBlksAlloc(0), _schedBlkSz(0),\n\
          \        _nActive(0), _nStable(0), _nextSchedBlk(0),\n\
          \        _workers()\n\
          \    {\n\
          \        pthread_mutex_init (&this->_lock, nullptr);\n\
          \    }\n\
          \    ~strand_array ();\n\
          \\n\
          \    uint32_t in_state_index () const { return this->_inIdx; }\n\
          \\n\
          \    uint32_t num_active () const { return this->_nActive; }\n\
          \    uint32_t num_stable () const { return this->_nStable; }\n\
          \    uint32_t num_alive () const { return this->_nActive+this->_nStable; }\n\
          \\n\
          \  // return the ID of a strand, which is the value of the _idx array\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nStrands);\n\
          \        return this->_idx[ix];\n\
          \    }\n\
          \  // direct indexing of strands by ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        return id;\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nStrands);\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRAND@_local *local_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_local);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRAND@_local *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return &(this->id_to_strand(id)->_local);\n\
          \    }\n\
          \\n\
          \  // return a pointer to the in-state of strand ix\n\
          \    const @STRAND@_shared *in_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_shared[this->_inIdx]);\n\
          \    }\n\
          \  // return a pointer to the in-state of the strand with the given ID\n\
          \    const @STRAND@_shared *id_to_in_state (sid_t id) const\n\
          \    {\n\
          \        return &(this->id_to_strand(id)->_shared[this->_inIdx]);\n\
          \    }\n\
          \\n\
          \  // return a pointer to the out-state of strand ix\n\
          \    @STRAND@_shared *out_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_shared[this->_inIdx ^ 1]);\n\
          \    }\n\
          \\n\
          \  // deallocate space reserved for strands\n\
          \    void dealloc ();\n\
          \\n\
          \  // set the scheduling block size based on the number of workers and the number of\n\
          \  // strands.  This should be called before alloc.\n\
          \    void set_block_size (uint32_t nWorkers, uint32_t nStrands)\n\
          \    {\n\
          \        this->_schedBlkSz = diderot::sched_block_size (nWorkers, nStrands);\n\
          \    }\n\
          \\n\
          \  // allocate space for nItems organized into blkSz sized blocks of strands\n\
          \    bool alloc (uint32_t nItems);\n\
          \\n\
          \  // allocated a fresh block of storage for strand states\n\
          \    block_t *alloc_block ();\n\
          \\n\
          \  // initialize the first nStrands locations as new active strands\n\
          \    void create_strands (uint32_t nStrands);\n\
          \\n\
          \  // swap in and out states\n\
          \    void swap ()\n\
          \    {\n\
          \        this->_inIdx ^= 1;\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method (single-thread version)\n\
          \  // NOTE: because this function does not preserve the sched_block\n\
          \  // layout invariants, it should only be used for stabilize_all.\n\
          \    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];\n\
          \        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // note that we swap out and in here because out holds the current state\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);\n\
          \        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));\n\
          \#else\n\
          \        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \        this->_status[ix] = diderot::kStable;\n\
          \        this->_nActive--;\n\
          \        this->_nStable++;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // mark the given strand as dead (single-thread version)\n\
          \    index_t kill (index_t ix)\n\
          \    {\n\
          \        this->_status[ix] = diderot::kDead;\n\
          \        this->_nActive--;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // prepare to run the workers\n\
          \    void prepare_run ()\n\
          \    {\n\
          \        this->_nextSchedBlk = 0;\n\
          \    }\n\
          \\n\
          \  // finish the local-phase of a superstep\n\
          \    bool finish_step ();\n\
          \\n\
          \#ifdef DIDEROT_HAS_KILL_ALL // need kill for when step limit expires\n\
          \  // finish a kill_all operation (NOP)\n\
          \    void finish_kill_all () { }\n\
          \#endif\n\
          \\n\
          \  // finish a stabilize_all operation (NOP)\n\
          \    void finish_stabilize_all () { }\n\
          \\n\
          \  // iterator over stable strands\n\
          \    index_t begin_stable () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nStrands) && (this->status(ix) != diderot::kStable)) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_stable () const { return this->_nStrands; }\n\
          \    index_t next_stable (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nStrands) && (this->status(ix) != diderot::kStable));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over active strands\n\
          \    index_t begin_active () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nStrands) && notActiveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_active () const { return this->_nStrands; }\n\
          \    index_t next_active (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over alive (active+stable) strands\n\
          \    index_t begin_alive () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nStrands) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_alive () const { return this->_nStrands; }\n\
          \    index_t next_alive (index_t &ix) const\n\
          \    {\n\
          \        ix++;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nStrands) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // grow the _idx, _status, and _schedBlks arrays\n\
          \    bool grow (uint32_t n);\n\
          \\n\
          \}; // struct strand_array\n\
          \\n\
          \// allocate space for nItems organized into blkSz sized blocks of strands\n\
          \bool strand_array::alloc (uint32_t nItems)\n\
          \{\n\
          \    if (this->_schedBlkSz == 0) {\n\
          \        std::cerr << \"Internal error: strand_array block size is 0\\n\";\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \// FIXME: if the strands have sequences in them, then we need to invoke \"new\"!\n\
          \\n\
          \  // round number of items up to size of storage block\n\
          \    uint32_t arraySz = (nItems + BLKSZ - 1) & ~BLKMASK;\n\
          \    uint32_t nBlks = arraySz >> LOG_BLKSZ;\n\
          \    assert (arraySz == nBlks*BLKSZ);\n\
          \  // allocate block vector\n\
          \    this->_blocks.resize(nBlks, nullptr);\n\
          \  // allocate blocks of storage for strands\n\
          \    for (int i = 0;  i < nBlks;  i++) {\n\
          \        this->_blocks[i] = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));\n\
          \        if (this->_blocks[i] == nullptr) {\n\
          \          // unable to allocate memory\n\
          \            this->dealloc();\n\
          \            return true;\n\
          \        }\n\
          \    }\n\
          \\n\
          \  // allocate _idx, _status, and _schedBlks arrays\n\
          \    if (this->grow (arraySz)) {\n\
          \      // unable to allocate memory\n\
          \        this->dealloc();\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \  // initialize arrays\n\
          \    index_t ix = 0;\n\
          \    for (int i = 0;  i < nBlks;  i++) {\n\
          \        strand_t *p = reinterpret_cast<strand_t *>(this->_blocks[i]);\n\
          \        for (int j = 0;  j < BLKSZ;  j++, ix++) {\n\
          \            this->_status[ix] = diderot::kDead;\n\
          \            this->_idx[ix] = p++;\n\
          \        }\n\
          \    }\n\
          \\n\
          \    this->_arraySz = arraySz;\n\
          \    this->_nStrands = nItems;\n\
          \    this->_nActive = 0;\n\
          \    this->_nStable = 0;\n\
          \\n\
          \    return false;\n\
          \}\n\
          \\n\
          \strand_array::~strand_array ()\n\
          \{\n\
          \    pthread_mutex_destroy (&this->_lock);\n\
          \  // run destructors to reclaim any dynamic memory attached to the strand state\n\
          \    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {\n\
          \        this->strand(ix)->~@STRANDTY@();\n\
          \    }\n\
          \    this->dealloc();\n\
          \}\n\
          \\n\
          \void strand_array::dealloc ()\n\
          \{\n\
          \    if (this->_status != nullptr) {\n\
          \        std::free (this->_status);\n\
          \        this->_status = nullptr;\n\
          \    }\n\
          \    if (this->_idx != nullptr) {\n\
          \        std::free (this->_idx);\n\
          \        this->_idx = nullptr;\n\
          \    }\n\
          \    if (this->_schedBlks != nullptr) {\n\
          \        std::free (this->_schedBlks);\n\
          \        this->_schedBlks = nullptr;\n\
          \    }\n\
          \    for (uint32_t i = 0;  i < this->_blocks.size();  i++) {\n\
          \        if (this->_blocks[i] != nullptr) {\n\
          \            std::free (this->_blocks[i]);\n\
          \            this->_blocks[i] = nullptr;\n\
          \        }\n\
          \        else {\n\
          \            break;\n\
          \        }\n\
          \    }\n\
          \}\n\
          \\n\
          \// initialize the first nStrands locations as new active strands\n\
          \void strand_array::create_strands (uint32_t nStrands)\n\
          \{\n\
          \    assert (this->_nActive == 0);\n\
          \    assert (this->_arraySz >= nStrands);\n\
          \    assert (this->_nStrands == nStrands);\n\
          \    for (index_t ix = 0;  ix < nStrands;  ix++) {\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \        this->_status[ix] = diderot::kNew;\n\
          \#else\n\
          \        this->_status[ix] = diderot::kActive;\n\
          \#endif\n\
          \        new (this->strand(ix)) @STRANDTY@;\n\
          \    }\n\
          \    this->_nActive = nStrands;\n\
          \//    this->_nFresh = nStrands;\n\
          \  // initialize the scheduling blocks\n\
          \    uint32_t lastBlk = nStrands / this->_schedBlkSz;  // index of last in-use block\n\
          \    index_t ix = 0;\n\
          \    for (uint32_t i = 0;  i <= lastBlk;  i++) {\n\
          \        this->_schedBlks[i]._start = ix;\n\
          \        ix += this->_schedBlkSz;\n\
          \        this->_schedBlks[i]._stop = ix;\n\
          \        this->_schedBlks[i]._nDead = 0;\n\
          \        this->_schedBlks[i]._nStable = 0;\n\
          \    }\n\
          \  // adjust the number of dead strands in the last block to account for unused stands\n\
          \    this->_schedBlks[lastBlk]._nDead = this->_schedBlks[lastBlk]._stop - nStrands;\n\
          \    this->_nSchedBlks = lastBlk+1;\n\
          \\n\
          \}\n\
          \\n\
          \// grow the _idx, _status, and _schedBlks arrays to accomodate at least n additional\n\
          \// strands\n\
          \// Note that we do not need to allocate storage space for strands,\n\
          \// since that is handled by the workers\n\
          \bool strand_array::grow (uint32_t n)\n\
          \{\n\
          \  // round size of arrays to multiple of scheduler block size\n\
          \    size_t arraySz = static_cast<size_t>(this->_arraySz) + n + this->_schedBlkSz - 1;\n\
          \    arraySz &= ~(this->_schedBlkSz - 1);\n\
          \\n\
          \    if (arraySz >= UINT32_MAX) {\n\
          \      // cannot have more than UINT32_MAX elements\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \  // allocate enough scheduler blocks to cover all of the allocated status/idx items\n\
          \    uint32_t nSchedBlks = arraySz / this->_schedBlkSz;\n\
          \\n\
          \  // grow the arrays\n\
          \    uint8_t *status = static_cast<uint8_t *>(std::malloc (arraySz * sizeof(uint8_t)));\n\
          \    sid_t *idx = static_cast<sid_t *>(std::malloc (arraySz * sizeof(sid_t)));\n\
          \    sched_block *schedBlks = static_cast<sched_block *>(std::malloc(nSchedBlks * sizeof(sched_block)));\n\
          \    if ((status == nullptr) || (idx == nullptr) || (schedBlks == nullptr)) {\n\
          \        return true;\n\
          \    }\n\
          \    if (this->_arraySz > 0) {\n\
          \        std::memcpy (status, this->_status, this->_arraySz * sizeof(uint8_t));\n\
          \        std::memcpy (idx, this->_idx, this->_arraySz * sizeof(sid_t));\n\
          \        std::memcpy (schedBlks, this->_schedBlks, this->_nSchedBlksAlloc * sizeof(sched_block));\n\
          \      // free the old storage\n\
          \        std::free (this->_status);\n\
          \        std::free (this->_idx);\n\
          \        std::free (this->_schedBlks);\n\
          \    }\n\
          \\n\
          \  // initialize new sched_blocks\n\
          \    uint32_t blkIx = this->_nSchedBlksAlloc;\n\
          \    index_t ix = blkIx * this->_schedBlkSz;\n\
          \    for (; blkIx < nSchedBlks;  blkIx++) {\n\
          \        schedBlks[blkIx]._start = ix;\n\
          \        ix += this->_schedBlkSz;\n\
          \        schedBlks[blkIx]._stop = ix;\n\
          \        schedBlks[blkIx]._nStable = 0;\n\
          \        schedBlks[blkIx]._nDead = this->_schedBlkSz;\n\
          \    }\n\
          \\n\
          \  // update pointers etc.\n\
          \    this->_status = status;\n\
          \    this->_idx = idx;\n\
          \    this->_schedBlks = schedBlks;\n\
          \    this->_arraySz = arraySz;\n\
          \    this->_nSchedBlksAlloc = nSchedBlks;\n\
          \\n\
          \    return false;\n\
          \}\n\
          \\n\
          \// a local copy of strand state for workers\n\
          \struct worker_cache {\n\
          \    typedef strand_array::strand_t strand_t;\n\
          \    typedef strand_array::index_t index_t;\n\
          \    typedef strand_array::sid_t sid_t;\n\
          \    typedef strand_array::block_t block_t;\n\
          \    typedef strand_array::sched_block sched_block;\n\
          \\n\
          \    strand_array        *_sarray;       // pointer to global strand_array structure\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    sid_t               *_idx;          // array of strand indices for indirect state rep.\n\
          \    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling\n\
          \    atomic_uint32_t     *_nextBlkPtr;   // pointer to _nextSchedBlk\n\
          \    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)\n\
          \    uint32_t            _nStabilizing;  // count of strands run by this worker that stabilized in\n\
          \                                        // the current superstep\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    uint32_t            _nDying;        // count of strands run by this worker that died in\n\
          \                                        // the current superstep\n\
          \#endif\n\
          \    uint32_t            _nSchedBlks;    // number of scheduling blocks\n\
          \    uint32_t            _schedBlkSz;    // size of scheduling blocks\n\
          \#ifndef NDEBUG\n\
          \    uint32_t            _nStrands;      // number of strands in the _idx and _status arrays\n\
          \#endif\n\
          \    block_t             _newBlock;      // strand-storage block for new strands\n\
          \    strand_t            *_nextStrand;   // allocation pointer for new strands; should point inside\n\
          \                                        // the _newBlock\n\
          \    strand_t            *_limitPtr;     // limit pointer for new-strand allocation\n\
          \    std::vector<sid_t>  _fresh;         // fresh strands created in current superstep\n\
          \\n\
          \  // allocate a block of storage for new strands; returns true if there is an error\n\
          \    bool alloc_block ();\n\
          \\n\
          \    void init (strand_array &sarr)\n\
          \    {\n\
          \        this->_sarray = &sarr;\n\
          \        this->_status = sarr._status;\n\
          \        this->_idx = sarr._idx;\n\
          \        this->_schedBlks = sarr._schedBlks;\n\
          \        this->_nextBlkPtr = &sarr._nextSchedBlk;\n\
          \        this->_inIdx = sarr._inIdx;\n\
          \        this->_nStabilizing = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        this->_nDying = 0;\n\
          \#endif\n\
          \        this->_nSchedBlks = sarr._nSchedBlks;\n\
          \        this->_schedBlkSz = sarr._schedBlkSz;\n\
          \#ifndef NDEBUG\n\
          \        this->_nStrands = sarr._nStrands;\n\
          \#endif\n\
          \        this->_nextStrand = nullptr;\n\
          \        this->_limitPtr = nullptr;\n\
          \        sarr._workers.push_back (this);\n\
          \    }\n\
          \\n\
          \  // refresh those parts of the cache that might change between steps\n\
          \    void refresh ()\n\
          \    {\n\
          \        this->_status = this->_sarray->_status;\n\
          \        this->_idx = this->_sarray->_idx;\n\
          \        this->_nStabilizing = 0; /* QUESTION: is this the correct place for this? */\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        this->_nDying = 0;\n\
          \#endif\n\
          \        this->_schedBlks = this->_sarray->_schedBlks;\n\
          \        this->_nSchedBlks = this->_sarray->_nSchedBlks;\n\
          \#ifndef NDEBUG\n\
          \        this->_nStrands = this->_sarray->_nStrands;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \  // return the ID of a strand, which is the value of the _idx array\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nStrands);\n\
          \        return this->_idx[ix];\n\
          \    }\n\
          \  // direct indexing of strands by ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        return id;\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nStrands);\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \  // invoke strand's start method\n\
          \    diderot::strand_status strand_start (@START_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        return @STRAND@_start (@START_ARGS@\n\
          \            &self->_local,\n\
          \            &self->_shared[this->_inIdx],\n\
          \            &self->_shared[this->_inIdx^1]);\n\
          \    }\n\
          \\n\
          \    void run_start_methods (@START_PARAMS@sched_block *bp);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \\n\
          \  // invoke strand's update method\n\
          \    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        return @STRAND@_update (@UPDATE_ARGS@\n\
          \            &self->_local,\n\
          \            &self->_shared[this->_inIdx],\n\
          \            &self->_shared[this->_inIdx^1]);\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method (multithread version)\n\
          \    index_t strand_stabilize (sched_block *bp, @STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];\n\
          \        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // note that we swap out and in here because out holds the current state\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);\n\
          \        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));\n\
          \#else\n\
          \        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // we swap the strand-indices at ix and bp->_start + bp->_nStable\n\
          \        uint32_t jx = bp->_start + bp->_nStable;\n\
          \        this->_status[jx] = diderot::kStable;\n\
          \        std::swap (this->_idx[ix], this->_idx[jx]);\n\
          \        bp->_nStable++;\n\
          \        return ix+1;\n\
          \    }\n\
          \\n\
          \  // mark the given strand as dead (multithread version)\n\
          \    index_t kill (sched_block *bp, index_t ix)\n\
          \    {\n\
          \        assert (bp->_start + bp->_nStable <= ix);\n\
          \        assert (ix < bp->_start + bp->num_alive());\n\
          \        bp->_nDead++;\n\
          \      // swap the strand at ix with the last active strand in the block\n\
          \        uint32_t jx = bp->_stop - bp->_nDead;\n\
          \        this->_status[jx] = diderot::kDead;\n\
          \        std::swap (this->_idx[ix], this->_idx[jx]);\n\
          \        return ix;  // don't advance, since ix is an active strand after the swap\n\
          \    }\n\
          \\n\
          \  // wrappers for accessing the state of newly created strands\n\
          \    @STRAND@_local *new_local_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->id_to_strand(this->_fresh[ix])->_local);\n\
          \    }\n\
          \    @STRAND@_shared *new_out_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->id_to_strand(this->_fresh[ix])->_shared[this->_inIdx ^ 1]);\n\
          \    }\n\
          \\n\
          \    index_t new_strand ()\n\
          \    {\n\
          \        index_t ix = this->_fresh.size();\n\
          \        if (this->_nextStrand >= this->_limitPtr) {\n\
          \            if (this->alloc_block()) {\n\
          \                std::cerr << \"Fatal error: unable to allocate space for new strands\" << std::endl;\n\
          \                exit (1);\n\
          \            }\n\
          \        }\n\
          \        strand_t *strand = this->_nextStrand;\n\
          \        this->_nextStrand++;\n\
          \        this->_fresh.push_back (strand);\n\
          \        new (strand) @STRANDTY@;\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over active strands in a scheduling block\n\
          \    index_t begin_active (const sched_block *bp) const { return bp->_start + bp->_nStable; }\n\
          \    index_t end_active (const sched_block *bp) const { return bp->_stop - bp->_nDead; }\n\
          \    index_t next_active (const sched_block *bp, index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // iterator over fresh strands in a scheduling block\n\
          \    index_t begin_fresh (const sched_block *bp) const\n\
          \    {\n\
          \        index_t ix = this->begin_active(bp);\n\
          \        while ((ix != this->end_active(bp)) && (this->status(ix) != diderot::kNew)) {\n\
          \            ix = this->next_active(bp, ix);\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_fresh (const sched_block *bp) const { return this->end_active(bp); }\n\
          \    index_t next_fresh (const sched_block *bp, index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix = this->next_active(bp, ix);\n\
          \        } while ((ix != this->end_active(bp)) && (this->status(ix) != diderot::kNew));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // swap in and out states\n\
          \    void swap ()\n\
          \    {\n\
          \        this->_inIdx ^= 1;\n\
          \    }\n\
          \\n\
          \  // get a block of strands\n\
          \    sched_block *get_block ();\n\
          \\n\
          \}; // struct worker_cache\n\
          \\n\
          \strand_array::sched_block *worker_cache::get_block ()\n\
          \{\n\
          \    do {\n\
          \        uint32_t blkId = this->_nextBlkPtr->fetch_add(1);\n\
          \        if (blkId < this->_nSchedBlks) {\n\
          \            strand_array::sched_block *bp = &this->_schedBlks[blkId];\n\
          \            if (bp->num_active() > 0) {\n\
          \                return bp;\n\
          \            } // else skip stable block\n\
          \        }\n\
          \        else {  // no more blocks\n\
          \            return nullptr;\n\
          \        }\n\
          \    } while (true);\n\
          \\n\
          \}\n\
          \\n\
          \bool worker_cache::alloc_block ()\n\
          \{\n\
          \    pthread_mutex_lock(&this->_sarray->_lock);\n\
          \        char *blk = static_cast<block_t>(std::malloc (strand_array::BLKSZ * sizeof(@STRANDTY@)));\n\
          \        if (blk == nullptr) {\n\
          \            pthread_mutex_unlock(&this->_sarray->_lock);\n\
          \            return true;\n\
          \        }\n\
          \        this->_sarray->_blocks.push_back(blk);\n\
          \    pthread_mutex_unlock(&this->_sarray->_lock);\n\
          \\n\
          \    this->_newBlock = blk;\n\
          \    this->_nextStrand = reinterpret_cast<strand_t *>(blk);\n\
          \    this->_limitPtr = reinterpret_cast<strand_t *>(blk + strand_array::BLKSZ * sizeof(@STRANDTY@));\n\
          \\n\
          \    return false;\n\
          \}\n\
          \\n\
          \// finish the update phase of a superstep by compacting\n\
          \// strands and including any fresh strands from the other\n\
          \// workers.  Return true if there are any new or dead strands.\n\
          \bool strand_array::finish_step ()\n\
          \{\n\
          \    int32_t nStabilizing = 0;\n\
          \    int32_t nNew = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    int32_t nDying = 0;\n\
          \#endif\n\
          \\n\
          \    int32_t blkIx = 0;\n\
          \    for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {\n\
          \        worker_cache *wp = *it;\n\
          \        nStabilizing += wp->_nStabilizing;\n\
          \        nNew += wp->_fresh.size();\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        nDying += wp->_nDying;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \    if (nNew > 0) {\n\
          \      // the number of currently unused scheduler-block slots\n\
          \        index_t nAvail = this->_schedBlkSz * this->_nSchedBlksAlloc\n\
          \            - this->_nActive - this->_nStable;\n\
          \        if (nAvail < nNew) {\n\
          \          // we need to grow the _status, _idx, and _schedBlk arrays\n\
          \            this->grow (nNew - nAvail);\n\
          \        }\n\
          \        assert (this->_arraySz == this->_nSchedBlksAlloc * this->_schedBlkSz);\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        this->_nStrands += nNew - nDying;\n\
          \#else\n\
          \        this->_nStrands += nNew;\n\
          \#endif\n\
          \      // copy fresh strands into the unused slots\n\
          \        sched_block *bp = this->_schedBlks;\n\
          \        index_t nextIx = bp->next_avail();\n\
          \        uint32_t nBlks = 1;\n\
          \        for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {\n\
          \            worker_cache *wp = *it;\n\
          \            for (auto jt = wp->_fresh.begin();  jt != wp->_fresh.end();  ++jt) {\n\
          \              // advance to the next free slot\n\
          \                while (nextIx == bp->_stop) {\n\
          \                    bp++;\n\
          \                    nBlks++;\n\
          \                    nextIx = bp->next_avail();\n\
          \                }\n\
          \                assert (bp < this->_schedBlks + this->_nSchedBlksAlloc);\n\
          \                this->_idx[nextIx] = *jt;\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \                this->_status[nextIx] = diderot::kNew;\n\
          \#else\n\
          \                this->_status[nextIx] = diderot::kActive;\n\
          \#endif\n\
          \                nextIx++;\n\
          \                bp->_nDead--;\n\
          \            }\n\
          \            wp->_fresh.clear();\n\
          \        }\n\
          \        this->_nSchedBlks = nBlks;\n\
          \    }\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    else if (nDying > 0) {\n\
          \      /* FIXME: compact dead strands */\n\
          \/*\n\
          \      // check to see if we need to compact dead strands?\n\
          \        if ((this->_nStrands - this->_nActive) / this->_schedBlkSz > ??) {\n\
          \        }\n\
          \*/\n\
          \    }\n\
          \#endif\n\
          \\n\
          \  // reset scheduler for next superstep\n\
          \    this->_nextSchedBlk = 0;\n\
          \\n\
          \  // update global count of stable strands\n\
          \    this->_nStable += nStabilizing;\n\
          \  // update global count of active strands\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    this->_nActive += nNew - (nStabilizing + nDying);\n\
          \\n\
          \    return (nNew + nDying) > 0;\n\
          \#else\n\
          \    this->_nActive += nNew - nStabilizing;\n\
          \\n\
          \    return nNew > 0;\n\
          \#endif\n\
          \\n\
          \}\n\
          \/*---------- end par-sarr-dual-indirect.in ----------*/\n\
          \"

    val parSArrayDualDir = "\
          \/*---------- begin par-sarr-dual.in ----------*/\n\
          \// forward declaration of worker_cache type\n\
          \struct worker_cache;\n\
          \// forward declarations of strand methods\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_start (@START_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \static void @STRAND@_stabilize (@STABILIZE_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \#  error unexpected presence of \"die\"\n\
          \#endif\n\
          \\n\
          \// strand_array for PARALLEL_TARGET/BSP/DUAL STATE/DIRECT ACCESS\n\
          \//\n\
          \struct strand_array {\n\
          \    typedef @STRANDTY@ strand_t;\n\
          \    typedef uint32_t index_t;\n\
          \    typedef index_t sid_t;              // strand ID (index into strand-state storage)\n\
          \\n\
          \    // scheduling block of strands\n\
          \    //\n\
          \    struct CACHE_ALIGN sched_block {\n\
          \        index_t         _start;         // first index in block\n\
          \        index_t         _stop;          // last index in block + 1\n\
          \        uint32_t        _nStable;       // number of stable strands in the block\n\
          \\n\
          \      // return the number of strands in the block\n\
          \        uint32_t num_strands () const { return this->_stop - this->_start; }\n\
          \      // return the number of active strands in the block\n\
          \        uint32_t num_active () const\n\
          \        {\n\
          \            return this->num_strands() - this->_nStable;\n\
          \        }\n\
          \    };\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    char                *_storage;      // points to array of @STRANDTY@ structs\n\
          \    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)\n\
          \    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling\n\
          \    uint32_t            _nItems;        // number of items in the _storage and _status arrays\n\
          \    uint32_t            _nFresh;        // number of fresh strands (new strands from create_strands)\n\
          \    uint32_t            _nBlks;         // number of scheduling blocks\n\
          \    uint32_t            _blkSz;         // size of scheduling blocks\n\
          \    atomic_uint32_t     _nStable CACHE_ALIGN;\n\
          \                                        // global number of stable strands\n\
          \    atomic_uint32_t     _nActive CACHE_ALIGN;\n\
          \                                        // global number of active strands\n\
          \    atomic_uint32_t     _nextSchedBlk CACHE_ALIGN;\n\
          \                                        // next block to schedule\n\
          \    std::vector<worker_cache *> _workers;\n\
          \\n\
          \    strand_array ()\n\
          \        : _status(nullptr), _storage(nullptr), _schedBlks(nullptr), _nItems(0),\n\
          \          _nStable(0), _nActive(0), _nFresh(0), _nBlks(0), _blkSz(0), _nextSchedBlk(0)\n\
          \    { }\n\
          \    ~strand_array ();\n\
          \\n\
          \    uint32_t in_state_index () const { return this->_inIdx; }\n\
          \\n\
          \    uint32_t num_active () const { return this->_nActive; }\n\
          \    uint32_t num_stable () const { return this->_nStable; }\n\
          \    uint32_t num_alive () const { return this->_nActive+this->_nStable; }\n\
          \\n\
          \  // return the ID of a strand, which is the value of the _idx array\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return ix;\n\
          \    }\n\
          \  // return a pointer to the strand with the given ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        assert (id < this->_nItems);\n\
          \        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRAND@_local *local_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_local);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRAND@_local *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return &(this->id_to_strand(id)->_local);\n\
          \    }\n\
          \  // return a pointer to the in-state of strand ix\n\
          \    const @STRAND@_shared *in_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_shared[this->_inIdx]);\n\
          \    }\n\
          \  // return a pointer to the in-state of the strand with the given ID\n\
          \    const @STRAND@_shared *id_to_in_state (sid_t id) const\n\
          \    {\n\
          \        return &(this->id_to_strand(id)->_shared[this->_inIdx]);\n\
          \    }\n\
          \  // return a pointer to the out-state of strand ix\n\
          \    @STRAND@_shared *out_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_shared[this->_inIdx ^ 1]);\n\
          \    }\n\
          \\n\
          \  // set the scheduling block size based on the number of workers and the number of\n\
          \  // strands.  This should be called before alloc.\n\
          \    void set_block_size (uint32_t nWorkers, uint32_t nStrands)\n\
          \    {\n\
          \        this->_blkSz = diderot::sched_block_size (nWorkers, nStrands);\n\
          \    }\n\
          \\n\
          \  // allocate space for nItems organized into blkSz sized blocks of strands\n\
          \    bool alloc (uint32_t nItems);\n\
          \\n\
          \  // initialize the first nStrands locations as new active strands\n\
          \    void create_strands (uint32_t nStrands);\n\
          \\n\
          \  // swap in and out states\n\
          \    void swap ()\n\
          \    {\n\
          \        this->_inIdx ^= 1;\n\
          \// FIXME: once we have parallel reductions and parallel tree building, we will need\n\
          \// to reset this counter in other places too\n\
          \        this->_nextSchedBlk = 0;\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method (single-thread version)\n\
          \    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];\n\
          \        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // note that we swap out and in here because out holds the current state\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);\n\
          \        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));\n\
          \#else\n\
          \        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \        this->_status[ix] = diderot::kStable;\n\
          \        this->_nActive--;\n\
          \        this->_nStable++;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_KILL_ALL // need kill for when step limit expires\n\
          \  // mark the given strand as dead (single-thread version)\n\
          \    index_t kill (index_t ix)\n\
          \    {\n\
          \        this->_status[ix] = diderot::kDead;\n\
          \        this->_nActive--;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \#endif\n\
          \\n\
          \  // prepare to run the workers\n\
          \    void prepare_run ()\n\
          \    {\n\
          \        this->_nextSchedBlk = 0;\n\
          \    }\n\
          \\n\
          \  // finish the local-phase of a superstep\n\
          \    bool finish_step ();\n\
          \\n\
          \#ifdef DIDEROT_HAS_KILL_ALL // need kill for when step limit expires\n\
          \  // finish a kill_all operation (NOP)\n\
          \    void finish_kill_all () { }\n\
          \#endif\n\
          \\n\
          \  // finish a stabilize_all operation (NOP)\n\
          \    void finish_stabilize_all () { }\n\
          \\n\
          \  // iterator over all alive strands (single-threaded version)\n\
          \    index_t begin_alive () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_alive () const { return this->_nItems; }\n\
          \    index_t next_alive (index_t &ix) const\n\
          \    {\n\
          \        ix++;\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over all active strands (single-threaded version)\n\
          \    index_t begin_active () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nItems) && notActiveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_active () const { return this->_nItems; }\n\
          \    index_t next_active (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over stable strands\n\
          \    index_t begin_stable () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable)) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_stable () const { return this->_nItems; }\n\
          \    index_t next_stable (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over fresh strands; since the only new strands were created by create_strand\n\
          \  // we iterate over all of them\n\
          \    index_t begin_fresh () const { return 0; }\n\
          \    index_t end_fresh () const { return this->_nFresh; }\n\
          \    index_t next_fresh (index_t &ix) const { return ++ix; }\n\
          \\n\
          \}; // struct strand_array\n\
          \\n\
          \strand_array::~strand_array ()\n\
          \{\n\
          \  // run destructors to reclaim any dynamic memory attached to the strand state\n\
          \    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {\n\
          \        this->strand(ix)->~@STRANDTY@();\n\
          \    }\n\
          \    if (this->_status != nullptr) std::free (this->_status);\n\
          \    if (this->_storage != nullptr) std::free (this->_storage);\n\
          \    if (this->_schedBlks != nullptr) std::free (this->_schedBlks);\n\
          \}\n\
          \\n\
          \bool strand_array::alloc (uint32_t nItems)\n\
          \{\n\
          \    if (this->_blkSz == 0) {\n\
          \        std::cerr << \"Internal error: strand_array block size is 0\\n\";\n\
          \        return true;\n\
          \    }\n\
          \    this->_storage = static_cast<char *>(std::malloc (nItems * sizeof(@STRANDTY@)));\n\
          \    if (this->_storage == nullptr) {\n\
          \        return true;\n\
          \    }\n\
          \    this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));\n\
          \    if (this->_status == nullptr) {\n\
          \        std::free (this->_storage);\n\
          \        return true;\n\
          \    }\n\
          \    this->_nBlks = (nItems + this->_blkSz - 1) / this->_blkSz;\n\
          \    this->_schedBlks = static_cast<sched_block *>(std::malloc (this->_nBlks * sizeof(sched_block)));\n\
          \    if (this->_schedBlks == nullptr) {\n\
          \        std::free (this->_storage);\n\
          \        std::free (this->_status);\n\
          \        return true;\n\
          \    }\n\
          \    this->_inIdx = 0;\n\
          \    this->_nItems = nItems;\n\
          \    this->_nActive = 0;\n\
          \    this->_nStable = 0;\n\
          \    this->_nFresh = 0;\n\
          \    return false;\n\
          \}\n\
          \\n\
          \void strand_array::create_strands (uint32_t nStrands)\n\
          \{\n\
          \    assert (this->_nActive == 0);\n\
          \    assert (this->_nItems == nStrands);\n\
          \    for (uint32_t ix = 0;  ix < nStrands;  ix++) {\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \        this->_status[ix] = diderot::kNew;\n\
          \#else\n\
          \        this->_status[ix] = diderot::kActive;\n\
          \#endif\n\
          \        new(this->strand(ix)) @STRANDTY@;\n\
          \    }\n\
          \    this->_nActive = nStrands;\n\
          \    this->_nFresh = nStrands;\n\
          \  // initialize the scheduling blocks\n\
          \    for (uint32_t ix = 0, i = 0;  i < this->_nBlks;  i++) {\n\
          \        this->_schedBlks[i]._start = ix;\n\
          \        ix += this->_blkSz;\n\
          \        this->_schedBlks[i]._stop = ix;\n\
          \        this->_schedBlks[i]._nStable = 0;\n\
          \    }\n\
          \  // the last block may be incomplete, so adjust it\n\
          \    this->_schedBlks[this->_nBlks-1]._stop = nStrands;\n\
          \}\n\
          \\n\
          \// a local copy of strand state for workers\n\
          \struct worker_cache {\n\
          \    typedef strand_array::strand_t strand_t;\n\
          \    typedef strand_array::index_t index_t;\n\
          \    typedef strand_array::sid_t sid_t;\n\
          \    typedef strand_array::sched_block sched_block;\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    char                *_storage;      // points to array of @STRANDTY@ structs\n\
          \    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling\n\
          \    atomic_uint32_t     *_nStablePtr;   // pointer to _nStable\n\
          \    atomic_uint32_t     *_nActivePtr;   // pointer to _nActive\n\
          \    atomic_uint32_t     *_nextBlkPtr;   // pointer to _nextSchedBlk\n\
          \    uint32_t            _nStabilizing;  // count of strands run by this worker that stabilized in\n\
          \                                        // the current superstep\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    uint32_t            _nDying;        // count of strands run by this worker that died in\n\
          \                                        // the current superstep\n\
          \#endif\n\
          \    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)\n\
          \    uint32_t            _nBlks;         // number of scheduling blocks\n\
          \    uint32_t            _blkSz;         // size of scheduling blocks\n\
          \#ifndef NDEBUG\n\
          \    uint32_t        _nItems;        // number of items in the _storage and _status arrays\n\
          \#endif\n\
          \\n\
          \    void init (strand_array &sarr)\n\
          \    {\n\
          \        this->_status = sarr._status;\n\
          \        this->_storage = sarr._storage;\n\
          \        this->_schedBlks = sarr._schedBlks;\n\
          \        this->_nStablePtr = &sarr._nStable;\n\
          \        this->_nActivePtr = &sarr._nActive;\n\
          \        this->_nextBlkPtr = &sarr._nextSchedBlk;\n\
          \        this->_nStabilizing = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        this->_nDying = 0;\n\
          \#endif\n\
          \        this->_inIdx = sarr._inIdx;\n\
          \        this->_nBlks = sarr._nBlks;\n\
          \        this->_blkSz = sarr._blkSz;\n\
          \#ifndef NDEBUG\n\
          \        this->_nItems = sarr._nItems;\n\
          \#endif\n\
          \        sarr._workers.push_back (this);\n\
          \    }\n\
          \\n\
          \  // refresh those parts of the cache that might change between steps\n\
          \    void refresh ()\n\
          \    {\n\
          \        // this target does not support dynamic strands, so nothing can change\n\
          \    }\n\
          \\n\
          \  // return the ID of a strand, which is the value of the _idx array\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return ix;\n\
          \    }\n\
          \  // return a pointer to the strand with the given ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));\n\
          \    }\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \\n\
          \  // swap in and out states\n\
          \    void swap ()\n\
          \    {\n\
          \        this->_inIdx ^= 1;\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \  // invoke strand's start method\n\
          \    diderot::strand_status strand_start (@START_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        return @STRAND@_start (@START_ARGS@\n\
          \            &self->_local,\n\
          \            &self->_shared[this->_inIdx],\n\
          \            &self->_shared[this->_inIdx^1]);\n\
          \    }\n\
          \\n\
          \    void run_start_methods (@START_PARAMS@sched_block *bp);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \\n\
          \  // invoke strand's update method\n\
          \    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        return @STRAND@_update (@UPDATE_ARGS@\n\
          \            &self->_local,\n\
          \            &self->_shared[this->_inIdx],\n\
          \            &self->_shared[this->_inIdx^1]);\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method (multithread version)\n\
          \    index_t strand_stabilize (sched_block *bp, @STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];\n\
          \        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // note that we swap out and in here because out holds the current state\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);\n\
          \        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));\n\
          \#else\n\
          \        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \        this->_status[ix] = diderot::kStable;\n\
          \        bp->_nStable++;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over alive strands in a scheduling block\n\
          \    index_t begin_alive (const sched_block *bp) const\n\
          \    {\n\
          \        index_t ix = bp->_start;\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_alive (const sched_block *bp) const { return bp->_stop; }\n\
          \    index_t next_alive (const sched_block *bp, index_t &ix) const\n\
          \    {\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over active strands in a scheduling block\n\
          \    index_t begin_active (const sched_block *bp) const\n\
          \    {\n\
          \        index_t ix = bp->_start;\n\
          \        while ((ix < bp->_stop) && notActiveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_active (const sched_block *bp) const { return bp->_stop; }\n\
          \    index_t next_active (const sched_block *bp, index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over fresh strands in a scheduling block\n\
          \    index_t begin_fresh (const sched_block *bp) const\n\
          \    {\n\
          \        index_t ix = bp->_start;\n\
          \        while ((ix < bp->_stop) && (this->status(ix) != diderot::kNew)) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_fresh (const sched_block *bp) const { return bp->_stop; }\n\
          \    index_t next_fresh (const sched_block *bp, index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < bp->_stop) && (this->status(ix) != diderot::kNew));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // get a block of strands\n\
          \    sched_block *get_block ();\n\
          \\n\
          \}; // struct worker_cache\n\
          \\n\
          \strand_array::sched_block *worker_cache::get_block ()\n\
          \{\n\
          \    do {\n\
          \        uint32_t blkId = this->_nextBlkPtr->fetch_add(1);\n\
          \        if (blkId < this->_nBlks) {\n\
          \            strand_array::sched_block *bp = &this->_schedBlks[blkId];\n\
          \            if (bp->num_active() > 0) {\n\
          \                return bp;\n\
          \            } // else skip stable block\n\
          \        }\n\
          \        else {  // no more blocks\n\
          \            return nullptr;\n\
          \        }\n\
          \    } while (true);\n\
          \\n\
          \}\n\
          \\n\
          \// finish the update phase of a superstep.    Return true if there are any dead strands.\n\
          \bool strand_array::finish_step ()\n\
          \{\n\
          \    int32_t nStabilizing = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    int32_t nDying = 0;\n\
          \#endif\n\
          \\n\
          \    for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {\n\
          \        worker_cache *wp = *it;\n\
          \        nStabilizing += wp->_nStabilizing;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        nDying += wp->_nDying;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    if (nDying > 0) {\n\
          \      /* FIXME: compact dead strands */\n\
          \/*\n\
          \      // check to see if we need to compact dead strands?\n\
          \        if ((this->_nStrands - this->_nActive) / this->_schedBlkSz > ??) {\n\
          \        }\n\
          \*/\n\
          \    }\n\
          \#endif\n\
          \\n\
          \  // reset scheduler for next superstep\n\
          \    this->_nextSchedBlk = 0;\n\
          \\n\
          \  // update global count of stable strands\n\
          \    this->_nStable += nStabilizing;\n\
          \  // update global count of active strands\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    this->_nActive -= (nStabilizing + nDying);\n\
          \\n\
          \    return (nDying > 0);\n\
          \#else\n\
          \    this->_nActive -= nStabilizing;\n\
          \\n\
          \    return false;\n\
          \#endif\n\
          \\n\
          \}\n\
          \/*---------- end par-sarr-dual.in ----------*/\n\
          \"

    val parSArrayInd = "\
          \/*---------- begin par-sarr-indirect.in ----------*/\n\
          \// forward declaration of worker_cache type\n\
          \struct worker_cache;\n\
          \// forward declarations of strand methods\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_start (@START_PARAMS@@STRANDTY@ *self);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@@STRANDTY@ *self);\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \static void @STRAND@_stabilize (@STABILIZE_PARAMS@@STRANDTY@ *self);\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \\n\
          \// strand_array for PARALLEL_TARGET/BSP/SINGLE STATE/INDIRECT ACCESS\n\
          \//\n\
          \struct strand_array {\n\
          \    typedef @STRANDTY@ strand_t;\n\
          \    typedef uint32_t index_t;           // strand index (index into _idx and _status arrays)\n\
          \    typedef strand_t *sid_t;            // strand ID (pointer to strand-state storage)\n\
          \    typedef char *block_t;              // points to array of @STRANDTY@ structs\n\
          \\n\
          \    // scheduling block of strands\n\
          \    //\n\
          \    struct CACHE_ALIGN sched_block {\n\
          \        index_t         _start;         // first index in block\n\
          \        index_t         _stop;          // last index in block + 1\n\
          \        uint32_t        _nStable;       // number of stable strands in the block\n\
          \        uint32_t        _nDead;         // number of dead strands in the block; this will\n\
          \                                        // be equal to the block size for unused blocks\n\
          \      // we organize the strands in a sched_block so that the stable strands are at\n\
          \      // the beginning, followed by the active strands, followed by the dead strands.\n\
          \      // An unused block will have _nDead == num_strands()\n\
          \\n\
          \      // return the number of strands in the block\n\
          \        uint32_t num_strands () const { return this->_stop - this->_start; }\n\
          \      // return the number of alive strands in the block\n\
          \        uint32_t num_alive () const\n\
          \        {\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \            return this->num_strands() - this->_nDead;\n\
          \#else\n\
          \            return this->num_strands();\n\
          \#endif\n\
          \        }\n\
          \      // return the number of active strands in the block\n\
          \        uint32_t num_active () const\n\
          \        {\n\
          \            return this->num_alive() - this->_nStable;\n\
          \        }\n\
          \      // return index of next available slot in block (_stop if none)\n\
          \        index_t next_avail () const { return this->_stop - this->_nDead; }\n\
          \\n\
          \      // is the block being used?\n\
          \        bool in_use () const { return this->_nDead == this->num_strands(); }\n\
          \    };\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    sid_t               *_idx;          // array of strand indices for indirect state rep.\n\
          \    std::vector<block_t> _blocks;       // vector of pointers to strand-storage blocks\n\
          \    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling\n\
          \    uint32_t            _arraySz;       // the number of allocated items in _status and _idx arrays\n\
          \    uint32_t            _nStrands;      // number of strands in the _status and _idx arrays\n\
          \                                        // (including dead strands)\n\
          \    uint32_t            _nSchedBlks;    // number of scheduling blocks in use\n\
          \    uint32_t            _nSchedBlksAlloc; // number of allocated scheduling blocks\n\
          \                                        // INV: _arraySz == _nSchedBlksAlloc * _schedBlkSz.\n\
          \    uint32_t            _schedBlkSz;    // size of scheduling blocks\n\
          \    atomic_uint32_t     _nextSchedBlk CACHE_ALIGN;\n\
          \                                        // next block to schedule\n\
          \    uint32_t            _nActive;       // global number of active strands\n\
          \    uint32_t            _nStable;       // global number of stable strands\n\
          \    pthread_mutex_t     _lock;          // lock for managing access to _blocks vector\n\
          \    std::vector<worker_cache *> _workers;\n\
          \\n\
          \  // size info for block_t objects\n\
          \    static const uint32_t LOG_BLKSZ = 12;               // 2^12 items per block\n\
          \    static const uint32_t BLKSZ = (1 << LOG_BLKSZ);\n\
          \    static const uint32_t BLKMASK = (BLKSZ-1);          // mask for block index\n\
          \\n\
          \    strand_array ()\n\
          \      : _status(nullptr), _idx(nullptr), _blocks(), _schedBlks(nullptr),\n\
          \        _arraySz(0), _nStrands(0), _nSchedBlks(0), _nSchedBlksAlloc(0), _schedBlkSz(0),\n\
          \        _nActive(0), _nStable(0), _nextSchedBlk(0),\n\
          \        _workers()\n\
          \    {\n\
          \        pthread_mutex_init (&this->_lock, nullptr);\n\
          \    }\n\
          \    ~strand_array ();\n\
          \\n\
          \    uint32_t in_state_index () const { return 0; /* dummy */ }\n\
          \\n\
          \    uint32_t num_active () const { return this->_nActive; }\n\
          \    uint32_t num_stable () const { return this->_nStable; }\n\
          \    uint32_t num_alive () const { return this->_nActive+this->_nStable; }\n\
          \\n\
          \  // return the ID of a strand, which is the value of the _idx array\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nStrands);\n\
          \        return this->_idx[ix];\n\
          \    }\n\
          \  // direct indexing of strands by ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        return id;\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nStrands);\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRANDTY@ *local_state (index_t ix) const\n\
          \    {\n\
          \        return this->strand(ix);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRANDTY@ *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return this->id_to_strand(id);\n\
          \    }\n\
          \\n\
          \  // deallocate space reserved for strands\n\
          \    void dealloc ();\n\
          \\n\
          \  // set the scheduling block size based on the number of workers and the number of\n\
          \  // strands.  This should be called before alloc.\n\
          \    void set_block_size (uint32_t nWorkers, uint32_t nStrands)\n\
          \    {\n\
          \        this->_schedBlkSz = diderot::sched_block_size (nWorkers, nStrands);\n\
          \    }\n\
          \\n\
          \  // allocate space for nItems organized into blkSz sized blocks of strands\n\
          \    bool alloc (uint32_t nItems);\n\
          \\n\
          \  // allocated a fresh block of storage for strand states\n\
          \    block_t *alloc_block ();\n\
          \\n\
          \  // initialize the first nStrands locations as new active strands\n\
          \    void create_strands (uint32_t nStrands);\n\
          \\n\
          \  // swap in and out states (NOP for this version)\n\
          \    void swap () { }\n\
          \\n\
          \  // invoke strand's stabilize method (single-thread version)\n\
          \  // NOTE: because this function does not preserve the sched_block\n\
          \  // layout invariants, it should only be used for stabilize_all.\n\
          \    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \        this->_status[ix] = diderot::kStable;\n\
          \        this->_nActive--;\n\
          \        this->_nStable++;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // mark the given strand as dead (single-thread version)\n\
          \    index_t kill (index_t ix)\n\
          \    {\n\
          \        this->_status[ix] = diderot::kDead;\n\
          \        this->_nActive--;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // prepare to run the workers\n\
          \    void prepare_run ()\n\
          \    {\n\
          \        this->_nextSchedBlk = 0;\n\
          \    }\n\
          \\n\
          \  // finish the local-phase of a superstep\n\
          \    bool finish_step ();\n\
          \\n\
          \#ifdef DIDEROT_HAS_KILL_ALL // need kill for when step limit expires\n\
          \  // finish a kill_all operation (NOP)\n\
          \    void finish_kill_all () { }\n\
          \#endif\n\
          \\n\
          \  // finish a stabilize_all operation (NOP)\n\
          \    void finish_stabilize_all () { }\n\
          \\n\
          \  // iterator over stable strands\n\
          \    index_t begin_stable () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nStrands) && (this->status(ix) != diderot::kStable)) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_stable () const { return this->_nStrands; }\n\
          \    index_t next_stable (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nStrands) && (this->status(ix) != diderot::kStable));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over active strands\n\
          \    index_t begin_active () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nStrands) && notActiveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_active () const { return this->_nStrands; }\n\
          \    index_t next_active (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nStrands) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over alive (active+stable) strands\n\
          \    index_t begin_alive () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nStrands) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_alive () const { return this->_nStrands; }\n\
          \    index_t next_alive (index_t &ix) const\n\
          \    {\n\
          \        ix++;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nStrands) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // grow the _idx, _status, and _schedBlks arrays\n\
          \    bool grow (uint32_t n);\n\
          \\n\
          \}; // struct strand_array\n\
          \\n\
          \// allocate space for nItems organized into blkSz sized blocks of strands\n\
          \bool strand_array::alloc (uint32_t nItems)\n\
          \{\n\
          \    if (this->_schedBlkSz == 0) {\n\
          \        std::cerr << \"Internal error: strand_array block size is 0\\n\";\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \// FIXME: if the strands have sequences in them, then we need to invoke \"new\"!\n\
          \\n\
          \  // round number of items up to size of storage block\n\
          \    uint32_t arraySz = (nItems + BLKSZ - 1) & ~BLKMASK;\n\
          \    uint32_t nBlks = arraySz >> LOG_BLKSZ;\n\
          \    assert (arraySz == nBlks*BLKSZ);\n\
          \  // allocate block vector\n\
          \    this->_blocks.resize(nBlks, nullptr);\n\
          \  // allocate blocks of storage for strands\n\
          \    for (int i = 0;  i < nBlks;  i++) {\n\
          \        this->_blocks[i] = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));\n\
          \        if (this->_blocks[i] == nullptr) {\n\
          \          // unable to allocate memory\n\
          \            this->dealloc();\n\
          \            return true;\n\
          \        }\n\
          \    }\n\
          \\n\
          \  // allocate _idx, _status, and _schedBlks arrays\n\
          \    if (this->grow (arraySz)) {\n\
          \      // unable to allocate memory\n\
          \        this->dealloc();\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \  // initialize arrays\n\
          \    index_t ix = 0;\n\
          \    for (int i = 0;  i < nBlks;  i++) {\n\
          \        strand_t *p = reinterpret_cast<strand_t *>(this->_blocks[i]);\n\
          \        for (int j = 0;  j < BLKSZ;  j++, ix++) {\n\
          \            this->_status[ix] = diderot::kDead;\n\
          \            this->_idx[ix] = p++;\n\
          \        }\n\
          \    }\n\
          \\n\
          \    this->_arraySz = arraySz;\n\
          \    this->_nStrands = nItems;\n\
          \    this->_nActive = 0;\n\
          \    this->_nStable = 0;\n\
          \\n\
          \    return false;\n\
          \}\n\
          \\n\
          \strand_array::~strand_array ()\n\
          \{\n\
          \    pthread_mutex_destroy (&this->_lock);\n\
          \  // run destructors to reclaim any dynamic memory attached to the strand state\n\
          \    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {\n\
          \        this->strand(ix)->~@STRANDTY@();\n\
          \    }\n\
          \    this->dealloc();\n\
          \}\n\
          \\n\
          \void strand_array::dealloc ()\n\
          \{\n\
          \    if (this->_status != nullptr) {\n\
          \        std::free (this->_status);\n\
          \        this->_status = nullptr;\n\
          \    }\n\
          \    if (this->_idx != nullptr) {\n\
          \        std::free (this->_idx);\n\
          \        this->_idx = nullptr;\n\
          \    }\n\
          \    if (this->_schedBlks != nullptr) {\n\
          \        std::free (this->_schedBlks);\n\
          \        this->_schedBlks = nullptr;\n\
          \    }\n\
          \    for (uint32_t i = 0;  i < this->_blocks.size();  i++) {\n\
          \        if (this->_blocks[i] != nullptr) {\n\
          \            std::free (this->_blocks[i]);\n\
          \            this->_blocks[i] = nullptr;\n\
          \        }\n\
          \        else {\n\
          \            break;\n\
          \        }\n\
          \    }\n\
          \}\n\
          \\n\
          \// initialize the first nStrands locations as new active strands\n\
          \void strand_array::create_strands (uint32_t nStrands)\n\
          \{\n\
          \    assert (this->_nActive == 0);\n\
          \    assert (this->_arraySz >= nStrands);\n\
          \    assert (this->_nStrands == nStrands);\n\
          \    for (index_t ix = 0;  ix < nStrands;  ix++) {\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \        this->_status[ix] = diderot::kNew;\n\
          \#else\n\
          \        this->_status[ix] = diderot::kActive;\n\
          \#endif\n\
          \        new (this->strand(ix)) @STRANDTY@;\n\
          \    }\n\
          \    this->_nActive = nStrands;\n\
          \//    this->_nFresh = nStrands;\n\
          \  // initialize the scheduling blocks\n\
          \    uint32_t lastBlk = nStrands / this->_schedBlkSz;  // index of last in-use block\n\
          \    index_t ix = 0;\n\
          \    for (uint32_t i = 0;  i <= lastBlk;  i++) {\n\
          \        this->_schedBlks[i]._start = ix;\n\
          \        ix += this->_schedBlkSz;\n\
          \        this->_schedBlks[i]._stop = ix;\n\
          \        this->_schedBlks[i]._nDead = 0;\n\
          \        this->_schedBlks[i]._nStable = 0;\n\
          \    }\n\
          \  // adjust the number of dead strands in the last block to account for unused stands\n\
          \    this->_schedBlks[lastBlk]._nDead = this->_schedBlks[lastBlk]._stop - nStrands;\n\
          \    this->_nSchedBlks = lastBlk+1;\n\
          \\n\
          \}\n\
          \\n\
          \// grow the _idx, _status, and _schedBlks arrays to accomodate at least n additional\n\
          \// strands\n\
          \// Note that we do not need to allocate storage space for strands,\n\
          \// since that is handled by the workers\n\
          \bool strand_array::grow (uint32_t n)\n\
          \{\n\
          \  // round size of arrays to multiple of scheduler block size\n\
          \    size_t arraySz = static_cast<size_t>(this->_arraySz) + n + this->_schedBlkSz - 1;\n\
          \    arraySz &= ~(this->_schedBlkSz - 1);\n\
          \\n\
          \    if (arraySz >= UINT32_MAX) {\n\
          \      // cannot have more than UINT32_MAX elements\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \  // allocate enough scheduler blocks to cover all of the allocated status/idx items\n\
          \    uint32_t nSchedBlks = arraySz / this->_schedBlkSz;\n\
          \\n\
          \  // grow the arrays\n\
          \    uint8_t *status = static_cast<uint8_t *>(std::malloc (arraySz * sizeof(uint8_t)));\n\
          \    sid_t *idx = static_cast<sid_t *>(std::malloc (arraySz * sizeof(sid_t)));\n\
          \    sched_block *schedBlks = static_cast<sched_block *>(std::malloc(nSchedBlks * sizeof(sched_block)));\n\
          \    if ((status == nullptr) || (idx == nullptr) || (schedBlks == nullptr)) {\n\
          \        return true;\n\
          \    }\n\
          \    if (this->_arraySz > 0) {\n\
          \        std::memcpy (status, this->_status, this->_arraySz * sizeof(uint8_t));\n\
          \        std::memcpy (idx, this->_idx, this->_arraySz * sizeof(sid_t));\n\
          \        std::memcpy (schedBlks, this->_schedBlks, this->_nSchedBlksAlloc * sizeof(sched_block));\n\
          \      // free the old storage\n\
          \        std::free (this->_status);\n\
          \        std::free (this->_idx);\n\
          \        std::free (this->_schedBlks);\n\
          \    }\n\
          \\n\
          \  // initialize new sched_blocks\n\
          \    uint32_t blkIx = this->_nSchedBlksAlloc;\n\
          \    index_t ix = blkIx * this->_schedBlkSz;\n\
          \    for (; blkIx < nSchedBlks;  blkIx++) {\n\
          \        schedBlks[blkIx]._start = ix;\n\
          \        ix += this->_schedBlkSz;\n\
          \        schedBlks[blkIx]._stop = ix;\n\
          \        schedBlks[blkIx]._nStable = 0;\n\
          \        schedBlks[blkIx]._nDead = this->_schedBlkSz;\n\
          \    }\n\
          \\n\
          \  // update pointers etc.\n\
          \    this->_status = status;\n\
          \    this->_idx = idx;\n\
          \    this->_schedBlks = schedBlks;\n\
          \    this->_arraySz = arraySz;\n\
          \    this->_nSchedBlksAlloc = nSchedBlks;\n\
          \\n\
          \    return false;\n\
          \}\n\
          \\n\
          \// a local copy of strand state for workers\n\
          \struct worker_cache {\n\
          \    typedef strand_array::strand_t strand_t;\n\
          \    typedef strand_array::index_t index_t;\n\
          \    typedef strand_array::sid_t sid_t;\n\
          \    typedef strand_array::block_t block_t;\n\
          \    typedef strand_array::sched_block sched_block;\n\
          \\n\
          \    strand_array        *_sarray;       // pointer to global strand_array structure\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    sid_t               *_idx;          // array of strand indices for indirect state rep.\n\
          \    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling\n\
          \    atomic_uint32_t     *_nextBlkPtr;   // pointer to _nextSchedBlk\n\
          \    uint32_t            _nStabilizing;  // count of strands run by this worker that stabilized in\n\
          \                                        // the current superstep\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    uint32_t            _nDying;        // count of strands run by this worker that died in\n\
          \                                        // the current superstep\n\
          \#endif\n\
          \    uint32_t            _nSchedBlks;    // number of scheduling blocks\n\
          \    uint32_t            _schedBlkSz;    // size of scheduling blocks\n\
          \#ifndef NDEBUG\n\
          \    uint32_t            _nStrands;      // number of strands in the _idx and _status arrays\n\
          \#endif\n\
          \    block_t             _newBlock;      // strand-storage block for new strands\n\
          \    strand_t            *_nextStrand;   // allocation pointer for new strands; should point inside\n\
          \                                        // the _newBlock\n\
          \    strand_t            *_limitPtr;     // limit pointer for new-strand allocation\n\
          \    std::vector<sid_t>  _fresh;         // fresh strands created in current superstep\n\
          \\n\
          \  // allocate a block of storage for new strands; returns true if there is an error\n\
          \    bool alloc_block ();\n\
          \\n\
          \    void init (strand_array &sarr)\n\
          \    {\n\
          \        this->_sarray = &sarr;\n\
          \        this->_status = sarr._status;\n\
          \        this->_idx = sarr._idx;\n\
          \        this->_schedBlks = sarr._schedBlks;\n\
          \        this->_nextBlkPtr = &sarr._nextSchedBlk;\n\
          \        this->_nStabilizing = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        this->_nDying = 0;\n\
          \#endif\n\
          \        this->_nSchedBlks = sarr._nSchedBlks;\n\
          \        this->_schedBlkSz = sarr._schedBlkSz;\n\
          \#ifndef NDEBUG\n\
          \        this->_nStrands = sarr._nStrands;\n\
          \#endif\n\
          \        this->_nextStrand = nullptr;\n\
          \        this->_limitPtr = nullptr;\n\
          \        sarr._workers.push_back (this);\n\
          \    }\n\
          \\n\
          \  // refresh those parts of the cache that might change between steps\n\
          \    void refresh ()\n\
          \    {\n\
          \        this->_status = this->_sarray->_status;\n\
          \        this->_idx = this->_sarray->_idx;\n\
          \        this->_nStabilizing = 0; /* QUESTION: is this the correct place for this? */\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        this->_nDying = 0;\n\
          \#endif\n\
          \        this->_schedBlks = this->_sarray->_schedBlks;\n\
          \        this->_nSchedBlks = this->_sarray->_nSchedBlks;\n\
          \#ifndef NDEBUG\n\
          \        this->_nStrands = this->_sarray->_nStrands;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \  // return the ID of a strand, which is the value of the _idx array\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nStrands);\n\
          \        return this->_idx[ix];\n\
          \    }\n\
          \  // direct indexing of strands by ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        return id;\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nStrands);\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRANDTY@ *local_state (index_t ix) const\n\
          \    {\n\
          \        return this->strand(ix);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRANDTY@ *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return this->id_to_strand(id);\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \  // invoke strand's start method\n\
          \    diderot::strand_status strand_start (@START_PARAMS@index_t ix)\n\
          \    {\n\
          \        return @STRAND@_start(@START_ARGS@this->strand(ix));\n\
          \    }\n\
          \\n\
          \    void run_start_methods (@START_PARAMS@sched_block *bp);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \\n\
          \  // invoke strand's update method\n\
          \    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)\n\
          \    {\n\
          \        return @STRAND@_update(@UPDATE_ARGS@this->strand(ix));\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method (multithread version)\n\
          \    index_t strand_stabilize (sched_block *bp, @STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // we swap the strand-indices at ix and bp->_start + bp->_nStable\n\
          \        uint32_t jx = bp->_start + bp->_nStable;\n\
          \        this->_status[jx] = diderot::kStable;\n\
          \        std::swap (this->_idx[ix], this->_idx[jx]);\n\
          \        bp->_nStable++;\n\
          \        return ix+1;\n\
          \    }\n\
          \\n\
          \  // mark the given strand as dead (multithread version)\n\
          \    index_t kill (sched_block *bp, index_t ix)\n\
          \    {\n\
          \        assert (bp->_start + bp->_nStable <= ix);\n\
          \        assert (ix < bp->_start + bp->num_alive());\n\
          \        bp->_nDead++;\n\
          \      // swap the strand at ix with the last active strand in the block\n\
          \        uint32_t jx = bp->_stop - bp->_nDead;\n\
          \        this->_status[jx] = diderot::kDead;\n\
          \        std::swap (this->_idx[ix], this->_idx[jx]);\n\
          \        return ix;  // don't advance, since ix is an active strand after the swap\n\
          \    }\n\
          \\n\
          \  // return a pointer to the given newly allocated strand\n\
          \    @STRANDTY@ *new_strand_state (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->_fresh[ix]);\n\
          \    }\n\
          \\n\
          \    index_t new_strand ()\n\
          \    {\n\
          \        index_t ix = this->_fresh.size();\n\
          \        if (this->_nextStrand >= this->_limitPtr) {\n\
          \            if (this->alloc_block()) {\n\
          \                std::cerr << \"Fatal error: unable to allocate space for new strands\" << std::endl;\n\
          \                exit (1);\n\
          \            }\n\
          \        }\n\
          \        strand_t *strand = this->_nextStrand;\n\
          \        this->_nextStrand++;\n\
          \        this->_fresh.push_back (strand);\n\
          \        new (strand) @STRANDTY@;\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over active strands in a scheduling block\n\
          \    index_t begin_active (const sched_block *bp) const { return bp->_start + bp->_nStable; }\n\
          \    index_t end_active (const sched_block *bp) const { return bp->_stop - bp->_nDead; }\n\
          \    index_t next_active (const sched_block *bp, index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // iterator over fresh strands in a scheduling block\n\
          \    index_t begin_fresh (const sched_block *bp) const\n\
          \    {\n\
          \        index_t ix = this->begin_active(bp);\n\
          \        while ((ix != this->end_active(bp)) && (this->status(ix) != diderot::kNew)) {\n\
          \            ix = this->next_active(bp, ix);\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_fresh (const sched_block *bp) const { return this->end_active(bp); }\n\
          \    index_t next_fresh (const sched_block *bp, index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix = this->next_active(bp, ix);\n\
          \        } while ((ix != this->end_active(bp)) && (this->status(ix) != diderot::kNew));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // swap in and out states (NOP for this version)\n\
          \    void swap () { }\n\
          \\n\
          \  // get a block of strands\n\
          \    sched_block *get_block ();\n\
          \\n\
          \}; // struct worker_cache\n\
          \\n\
          \strand_array::sched_block *worker_cache::get_block ()\n\
          \{\n\
          \    do {\n\
          \        uint32_t blkId = this->_nextBlkPtr->fetch_add(1);\n\
          \        if (blkId < this->_nSchedBlks) {\n\
          \            strand_array::sched_block *bp = &this->_schedBlks[blkId];\n\
          \            if (bp->num_active() > 0) {\n\
          \                return bp;\n\
          \            } // else skip stable block\n\
          \        }\n\
          \        else {  // no more blocks\n\
          \            return nullptr;\n\
          \        }\n\
          \    } while (true);\n\
          \\n\
          \}\n\
          \\n\
          \bool worker_cache::alloc_block ()\n\
          \{\n\
          \    pthread_mutex_lock(&this->_sarray->_lock);\n\
          \        char *blk = static_cast<block_t>(std::malloc (strand_array::BLKSZ * sizeof(@STRANDTY@)));\n\
          \        if (blk == nullptr) {\n\
          \            pthread_mutex_unlock(&this->_sarray->_lock);\n\
          \            return true;\n\
          \        }\n\
          \        this->_sarray->_blocks.push_back(blk);\n\
          \    pthread_mutex_unlock(&this->_sarray->_lock);\n\
          \\n\
          \    this->_newBlock = blk;\n\
          \    this->_nextStrand = reinterpret_cast<strand_t *>(blk);\n\
          \    this->_limitPtr = reinterpret_cast<strand_t *>(blk + strand_array::BLKSZ * sizeof(@STRANDTY@));\n\
          \\n\
          \    return false;\n\
          \}\n\
          \\n\
          \// finish the update phase of a superstep by compacting\n\
          \// strands and including any fresh strands from the other\n\
          \// workers.  Return true if there are any new or dead strands.\n\
          \bool strand_array::finish_step ()\n\
          \{\n\
          \    int32_t nStabilizing = 0;\n\
          \    int32_t nNew = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    int32_t nDying = 0;\n\
          \#endif\n\
          \\n\
          \    int32_t blkIx = 0;\n\
          \    for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {\n\
          \        worker_cache *wp = *it;\n\
          \        nStabilizing += wp->_nStabilizing;\n\
          \        nNew += wp->_fresh.size();\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        nDying += wp->_nDying;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \    if (nNew > 0) {\n\
          \      // the number of currently unused scheduler-block slots\n\
          \        index_t nAvail = this->_schedBlkSz * this->_nSchedBlksAlloc\n\
          \            - this->_nActive - this->_nStable;\n\
          \        if (nAvail < nNew) {\n\
          \          // we need to grow the _status, _idx, and _schedBlk arrays\n\
          \            this->grow (nNew - nAvail);\n\
          \        }\n\
          \        assert (this->_arraySz == this->_nSchedBlksAlloc * this->_schedBlkSz);\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        this->_nStrands += nNew - nDying;\n\
          \#else\n\
          \        this->_nStrands += nNew;\n\
          \#endif\n\
          \      // copy fresh strands into the unused slots\n\
          \        sched_block *bp = this->_schedBlks;\n\
          \        index_t nextIx = bp->next_avail();\n\
          \        uint32_t nBlks = 1;\n\
          \        for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {\n\
          \            worker_cache *wp = *it;\n\
          \            for (auto jt = wp->_fresh.begin();  jt != wp->_fresh.end();  ++jt) {\n\
          \              // advance to the next free slot\n\
          \                while (nextIx == bp->_stop) {\n\
          \                    bp++;\n\
          \                    nBlks++;\n\
          \                    nextIx = bp->next_avail();\n\
          \                }\n\
          \                assert (bp < this->_schedBlks + this->_nSchedBlksAlloc);\n\
          \                this->_idx[nextIx] = *jt;\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \                this->_status[nextIx] = diderot::kNew;\n\
          \#else\n\
          \                this->_status[nextIx] = diderot::kActive;\n\
          \#endif\n\
          \                nextIx++;\n\
          \                bp->_nDead--;\n\
          \            }\n\
          \            wp->_fresh.clear();\n\
          \        }\n\
          \        this->_nSchedBlks = nBlks;\n\
          \    }\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    else if (nDying > 0) {\n\
          \      /* FIXME: compact dead strands */\n\
          \/*\n\
          \      // check to see if we need to compact dead strands?\n\
          \        if ((this->_nStrands - this->_nActive) / this->_schedBlkSz > ??) {\n\
          \        }\n\
          \*/\n\
          \    }\n\
          \#endif\n\
          \\n\
          \  // reset scheduler for next superstep\n\
          \    this->_nextSchedBlk = 0;\n\
          \\n\
          \  // update global count of stable strands\n\
          \    this->_nStable += nStabilizing;\n\
          \  // update global count of active strands\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    this->_nActive += nNew - (nStabilizing + nDying);\n\
          \\n\
          \    return (nNew + nDying) > 0;\n\
          \#else\n\
          \    this->_nActive += nNew - nStabilizing;\n\
          \\n\
          \    return nNew > 0;\n\
          \#endif\n\
          \\n\
          \}\n\
          \/*---------- end par-sarr-indirect.in ----------*/\n\
          \"

    val parSArrayDir = "\
          \/*---------- begin par-sarr.in ----------*/\n\
          \// forward declaration of worker_cache type\n\
          \struct worker_cache;\n\
          \// forward declarations of strand methods\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_start (@START_PARAMS@@STRANDTY@ *self);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@@STRANDTY@ *self);\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \static void @STRAND@_stabilize (@STABILIZE_PARAMS@@STRANDTY@ *self);\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \\n\
          \// strand_array for PARALLEL_TARGET/NO BSP/SINGLE STATE/DIRECT ACCESS\n\
          \//\n\
          \struct strand_array {\n\
          \    typedef @STRANDTY@ strand_t;\n\
          \    typedef uint32_t index_t;\n\
          \    typedef index_t sid_t;              // strand ID (index into strand-state storage)\n\
          \\n\
          \    // scheduling block of strands\n\
          \    //\n\
          \    struct CACHE_ALIGN sched_block {\n\
          \        index_t         _start;         // first index in block\n\
          \        index_t         _stop;          // last index in block + 1\n\
          \        uint32_t        _nStable;       // number of stable strands in the block\n\
          \        uint32_t        _nDead;         // number of dead strands in the block\n\
          \\n\
          \      // return the number of strands in the block\n\
          \        uint32_t num_strands () const { return this->_stop - this->_start; }\n\
          \      // return the number of active strands in the block\n\
          \        uint32_t num_active () const\n\
          \        {\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \            return this->num_strands() - (this->_nStable + this->_nDead);\n\
          \#else\n\
          \            return this->num_strands() - this->_nStable;\n\
          \#endif\n\
          \        }\n\
          \    };\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    char                *_storage;      // points to array of @STRANDTY@ structs\n\
          \    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling\n\
          \    uint32_t            _nItems;        // number of items in the _storage and _status arrays\n\
          \    uint32_t            _nStable;       // global number of stable strands\n\
          \    uint32_t            _nActive;       // global number of active strands\n\
          \    uint32_t            _nFresh;        // number of fresh strands (new strands from create_strands)\n\
          \    uint32_t            _nSchedBlks;    // number of scheduling blocks\n\
          \    uint32_t            _schedBlkSz;    // size of scheduling blocks\n\
          \    atomic_uint32_t     _nextSchedBlk CACHE_ALIGN;\n\
          \                                        // next block to schedule\n\
          \    std::vector<worker_cache *> _workers;\n\
          \\n\
          \    strand_array ()\n\
          \        : _status(nullptr), _storage(nullptr), _schedBlks(nullptr), _nItems(0),\n\
          \          _nStable(0), _nActive(0), _nFresh(0), _nSchedBlks(0), _schedBlkSz(0), _nextSchedBlk(0)\n\
          \    { }\n\
          \    ~strand_array ();\n\
          \\n\
          \    uint32_t in_state_index () const { return 0; /* dummy */ }\n\
          \\n\
          \    uint32_t num_active () const { return this->_nActive; }\n\
          \    uint32_t num_stable () const { return this->_nStable; }\n\
          \    uint32_t num_alive () const { return this->_nActive+this->_nStable; }\n\
          \\n\
          \  // return the ID of a strand, which is just the value of the argument\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return ix;\n\
          \    }\n\
          \  // return a pointer to the strand with the given ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        assert (id < this->_nItems);\n\
          \        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRANDTY@ *local_state (index_t ix) const\n\
          \    {\n\
          \        return this->strand(ix);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRANDTY@ *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return this->id_to_strand(id);\n\
          \    }\n\
          \\n\
          \  // set the scheduling block size based on the number of workers and the number of\n\
          \  // strands.  This should be called before alloc.\n\
          \    void set_block_size (uint32_t nWorkers, uint32_t nStrands)\n\
          \    {\n\
          \        this->_schedBlkSz = diderot::sched_block_size (nWorkers, nStrands);\n\
          \    }\n\
          \\n\
          \  // allocate space for nItems organized into blkSz sized blocks of strands\n\
          \    bool alloc (uint32_t nItems);\n\
          \\n\
          \  // initialize the first nStrands locations as new active strands\n\
          \    void create_strands (uint32_t nStrands);\n\
          \\n\
          \  // swap in and out states (NOP for this version)\n\
          \    void swap () { }\n\
          \\n\
          \  // invoke strand's stabilize method (single-thread version)\n\
          \    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \        this->_status[ix] = diderot::kStable;\n\
          \        this->_nActive--;\n\
          \        this->_nStable++;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // mark the given strand as dead (single-thread version)\n\
          \    index_t kill (index_t ix)\n\
          \    {\n\
          \        this->_status[ix] = diderot::kDead;\n\
          \        this->_nActive--;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // prepare to run the workers\n\
          \    void prepare_run ()\n\
          \    {\n\
          \        this->_nextSchedBlk = 0;\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_BSP\n\
          \  // finish the local-phase of a superstep; note that this function is only used\n\
          \  // when BSP is enabled.\n\
          \    bool finish_step ();\n\
          \#endif\n\
          \\n\
          \  // finish a kill_all operation (NOP)\n\
          \    void finish_kill_all () { }\n\
          \\n\
          \  // finish a stabilize_all operation (NOP)\n\
          \    void finish_stabilize_all () { }\n\
          \\n\
          \  // iterator over all alive strands (single-threaded version)\n\
          \    index_t begin_alive () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_alive () const { return this->_nItems; }\n\
          \    index_t next_alive (index_t &ix) const\n\
          \    {\n\
          \        ix++;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over all active strands (single-threaded version)\n\
          \    index_t begin_active () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nItems) && notActiveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_active () const { return this->_nItems; }\n\
          \    index_t next_active (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over stable strands\n\
          \    index_t begin_stable () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable)) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_stable () const { return this->_nItems; }\n\
          \    index_t next_stable (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over fresh strands; since the only new strands were created by create_strand\n\
          \  // we iterate over all of them\n\
          \    index_t begin_fresh () const { return 0; }\n\
          \    index_t end_fresh () const { return this->_nFresh; }\n\
          \    index_t next_fresh (index_t &ix) const { return ++ix; }\n\
          \\n\
          \}; // struct strand_array\n\
          \\n\
          \strand_array::~strand_array ()\n\
          \{\n\
          \  // run destructors to reclaim any dynamic memory attached to the strand state\n\
          \    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {\n\
          \        this->strand(ix)->~@STRANDTY@();\n\
          \    }\n\
          \    if (this->_status != nullptr) std::free (this->_status);\n\
          \    if (this->_storage != nullptr) std::free (this->_storage);\n\
          \    if (this->_schedBlks != nullptr) std::free (this->_schedBlks);\n\
          \}\n\
          \\n\
          \bool strand_array::alloc (uint32_t nItems)\n\
          \{\n\
          \    if (this->_schedBlkSz == 0) {\n\
          \        std::cerr << \"Internal error: strand_array block size is 0\\n\";\n\
          \        return true;\n\
          \    }\n\
          \    this->_storage = static_cast<char *>(std::malloc (nItems * sizeof(@STRANDTY@)));\n\
          \    if (this->_storage == nullptr) {\n\
          \        return true;\n\
          \    }\n\
          \    this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));\n\
          \    if (this->_status == nullptr) {\n\
          \        std::free (this->_storage);\n\
          \        return true;\n\
          \    }\n\
          \    this->_nSchedBlks = (nItems + this->_schedBlkSz - 1) / this->_schedBlkSz;\n\
          \    this->_schedBlks =\n\
          \        static_cast<sched_block *>(std::malloc (this->_nSchedBlks * sizeof(sched_block)));\n\
          \    if (this->_schedBlks == nullptr) {\n\
          \        std::free (this->_storage);\n\
          \        std::free (this->_status);\n\
          \        return true;\n\
          \    }\n\
          \    this->_nItems = nItems;\n\
          \    this->_nActive = 0;\n\
          \    this->_nStable = 0;\n\
          \    this->_nFresh = 0;\n\
          \    return false;\n\
          \}\n\
          \\n\
          \void strand_array::create_strands (uint32_t nStrands)\n\
          \{\n\
          \    assert (this->_nActive == 0);\n\
          \    assert (this->_nItems == nStrands);\n\
          \    for (uint32_t ix = 0;  ix < nStrands;  ix++) {\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \        this->_status[ix] = diderot::kNew;\n\
          \#else\n\
          \        this->_status[ix] = diderot::kActive;\n\
          \#endif\n\
          \        new(this->strand(ix)) @STRANDTY@;\n\
          \    }\n\
          \    this->_nActive = nStrands;\n\
          \    this->_nFresh = nStrands;\n\
          \  // initialize the scheduling blocks\n\
          \    for (uint32_t ix = 0, i = 0;  i < this->_nSchedBlks;  i++) {\n\
          \        this->_schedBlks[i]._start = ix;\n\
          \        ix += this->_schedBlkSz;\n\
          \        if (ix < nStrands) {\n\
          \            this->_schedBlks[i]._stop = ix;\n\
          \        }\n\
          \        else {\n\
          \            this->_schedBlks[i]._stop = nStrands;\n\
          \        }\n\
          \        this->_schedBlks[i]._nDead = 0;\n\
          \        this->_schedBlks[i]._nStable = 0;\n\
          \    }\n\
          \}\n\
          \\n\
          \// a local copy of strand state for workers\n\
          \struct worker_cache {\n\
          \    typedef strand_array::strand_t strand_t;\n\
          \    typedef strand_array::index_t index_t;\n\
          \    typedef strand_array::sid_t sid_t;\n\
          \    typedef strand_array::sched_block sched_block;\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    char                *_storage;      // points to array of @STRANDTY@ structs\n\
          \    sched_block         *_schedBlks;    // blocks of strands for parallel scheduling\n\
          \    atomic_uint32_t     *_nextBlkPtr;   // pointer to _nextSchedBlk\n\
          \    uint32_t            _nStabilizing;  // count of strands run by this worker that stabilized in\n\
          \                                        // the current superstep\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    uint32_t            _nDying;        // count of strands run by this worker that died in\n\
          \                                        // the current superstep\n\
          \#endif\n\
          \    uint32_t            _nSchedBlks;    // number of scheduling blocks\n\
          \    uint32_t            _schedBlkSz;    // size of scheduling blocks\n\
          \#ifndef NDEBUG\n\
          \    uint32_t        _nItems;            // number of items in the _storage and _status arrays\n\
          \#endif\n\
          \\n\
          \    void init (strand_array &sarr)\n\
          \    {\n\
          \        this->_status = sarr._status;\n\
          \        this->_storage = sarr._storage;\n\
          \        this->_schedBlks = sarr._schedBlks;\n\
          \        this->_nextBlkPtr = &sarr._nextSchedBlk;\n\
          \        this->_nSchedBlks = sarr._nSchedBlks;\n\
          \        this->_schedBlkSz = sarr._schedBlkSz;\n\
          \#ifndef NDEBUG\n\
          \        this->_nItems = sarr._nItems;\n\
          \#endif\n\
          \        sarr._workers.push_back (this);\n\
          \    }\n\
          \\n\
          \  // refresh those parts of the cache that might change between steps\n\
          \    void refresh ()\n\
          \    {\n\
          \        // this target does not support dynamic strands, so nothing can change\n\
          \    }\n\
          \\n\
          \  // return the ID of a strand, which is the value of the _idx array\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return ix;\n\
          \    }\n\
          \  // return a pointer to the strand with the given ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));\n\
          \    }\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRANDTY@ *local_state (index_t ix) const\n\
          \    {\n\
          \        return this->strand(ix);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRANDTY@ *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return this->id_to_strand(id);\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \  // invoke strand's start method\n\
          \    diderot::strand_status strand_start (@START_PARAMS@index_t ix)\n\
          \    {\n\
          \        return @STRAND@_start(@START_ARGS@this->strand(ix));\n\
          \    }\n\
          \\n\
          \    void run_start_methods (@START_PARAMS@sched_block *bp);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \\n\
          \  // invoke strand's update method\n\
          \    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)\n\
          \    {\n\
          \        return @STRAND@_update(@UPDATE_ARGS@this->strand(ix));\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method (multithread version)\n\
          \    index_t strand_stabilize (sched_block *bp, @STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \        this->_status[ix] = diderot::kStable;\n\
          \        bp->_nStable++;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // mark the given strand as dead (multithread version)\n\
          \    index_t kill (sched_block *bp, index_t ix)\n\
          \    {\n\
          \        this->_status[ix] = diderot::kDead;\n\
          \        bp->_nDead++;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over alive strands in a scheduling block\n\
          \    index_t begin_alive (const sched_block *bp) const\n\
          \    {\n\
          \        index_t ix = bp->_start;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < bp->_stop) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_alive (const sched_block *bp) const { return bp->_stop; }\n\
          \    index_t next_alive (const sched_block *bp, index_t &ix) const\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < bp->_stop) && notAliveSts(this->status(ix)));\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over active strands in a scheduling block\n\
          \    index_t begin_active (const sched_block *bp) const\n\
          \    {\n\
          \        index_t ix = bp->_start;\n\
          \        while ((ix < bp->_stop) && notActiveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_active (const sched_block *bp) const { return bp->_stop; }\n\
          \    index_t next_active (const sched_block *bp, index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < bp->_stop) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over fresh strands in a scheduling block\n\
          \    index_t begin_fresh (const sched_block *bp) const\n\
          \    {\n\
          \        index_t ix = bp->_start;\n\
          \        while ((ix < bp->_stop) && (this->status(ix) != diderot::kNew)) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_fresh (const sched_block *bp) const { return bp->_stop; }\n\
          \    index_t next_fresh (const sched_block *bp, index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < bp->_stop) && (this->status(ix) != diderot::kNew));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // swap in and out states (NOP for this version)\n\
          \    void swap () { }\n\
          \\n\
          \  // get a block of strands\n\
          \    sched_block *get_block ();\n\
          \\n\
          \}; // struct worker_cache\n\
          \\n\
          \strand_array::sched_block *worker_cache::get_block ()\n\
          \{\n\
          \    do {\n\
          \        uint32_t blkId = this->_nextBlkPtr->fetch_add(1);\n\
          \        if (blkId < this->_nSchedBlks) {\n\
          \            strand_array::sched_block *bp = &this->_schedBlks[blkId];\n\
          \            if (bp->num_active() > 0) {\n\
          \                return bp;\n\
          \            } // else skip stable block\n\
          \        }\n\
          \        else {  // no more blocks\n\
          \            return nullptr;\n\
          \        }\n\
          \    } while (true);\n\
          \\n\
          \}\n\
          \\n\
          \#ifdef DIDEROT_BSP\n\
          \// finish the update phase of a superstep.  Return true if there are any dead strands.\n\
          \bool strand_array::finish_step ()\n\
          \{\n\
          \    int32_t nStabilizing = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    int32_t nDying = 0;\n\
          \#endif\n\
          \\n\
          \    for (auto it = this->_workers.begin();  it != this->_workers.end();  ++it) {\n\
          \        worker_cache *wp = *it;\n\
          \        nStabilizing += wp->_nStabilizing;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        nDying += wp->_nDying;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    if (nDying > 0) {\n\
          \      /* FIXME: compact dead strands */\n\
          \/*\n\
          \      // check to see if we need to compact dead strands?\n\
          \        if ((this->_nStrands - this->_nActive) / this->_schedBlkSz > ??) {\n\
          \        }\n\
          \*/\n\
          \    }\n\
          \#endif\n\
          \\n\
          \  // reset scheduler for next superstep\n\
          \    this->_nextSchedBlk = 0;\n\
          \\n\
          \  // update global count of stable strands\n\
          \    this->_nStable += nStabilizing;\n\
          \  // update global count of active strands\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \    this->_nActive -= (nStabilizing + nDying);\n\
          \\n\
          \    return (nDying > 0);\n\
          \#else\n\
          \    this->_nActive -= nStabilizing;\n\
          \\n\
          \    return false;\n\
          \#endif\n\
          \\n\
          \}\n\
          \#endif // DIDEROT_BSP\n\
          \/*---------- end par-sarr.in ----------*/\n\
          \"

    val seqMain = "\
          \/*---------- begin seq-main.in ----------*/\n\
          \using namespace @PREFIX@;\n\
          \\n\
          \//! Main function for standalone sequential C target\n\
          \//\n\
          \int main (int argc, const char **argv)\n\
          \{\n\
          \    bool        timingFlg = false;      //! true if timing computation\n\
          \    uint32_t    stepLimit = 0;          //! limit on number of execution steps (0 means unlimited)\n\
          \    std::string printFile = \"-\";        //! file to direct printed output into\n\
          \#ifdef DIDEROT_EXEC_SNAPSHOT\n\
          \    uint32_t    snapshotPeriod = 0;     //! supersteps per snapshot; 0 means no snapshots\n\
          \#endif\n\
          \    uint32_t    nSteps = 0;             //! number of supersteps taken\n\
          \\n\
          \  // create the world\n\
          \    world *wrld = new (std::nothrow) world();\n\
          \    if (wrld == nullptr) {\n\
          \        std::cerr << \"unable to create world\" << std::endl;\n\
          \        exit(1);\n\
          \    }\n\
          \\n\
          \#ifndef DIDEROT_NO_INPUTS\n\
          \  // initialize the default values for the inputs\n\
          \    cmd_line_inputs inputs;\n\
          \    init_defaults (&inputs);\n\
          \#endif\n\
          \\n\
          \  // handle command-line options\n\
          \    {\n\
          \        diderot::options *opts = new diderot::options ();\n\
          \        opts->add (\"l,limit\", \"specify limit on number of super-steps (0 means unlimited)\",\n\
          \            &stepLimit, true);\n\
          \#ifdef DIDEROT_EXEC_SNAPSHOT\n\
          \        opts->add (\"s,snapshot\",\n\
          \            \"specify number of super-steps per snapshot (0 means no snapshots)\",\n\
          \            &snapshotPeriod, true);\n\
          \#endif\n\
          \        opts->add (\"print\", \"specify where to direct printed output\", &printFile, true);\n\
          \        opts->addFlag (\"v,verbose\", \"enable runtime-system messages\", &(wrld->_verbose));\n\
          \        opts->addFlag (\"t,timing\", \"enable execution timing\", &timingFlg);\n\
          \#ifndef DIDEROT_NO_INPUTS\n\
          \      // register options for setting global inputs\n\
          \        register_inputs (&inputs, opts);\n\
          \#endif\n\
          \        register_outputs (opts);\n\
          \        opts->process (argc, argv);\n\
          \        delete opts;\n\
          \    }\n\
          \\n\
          \  // redirect printing (if necessary)\n\
          \    if (printFile.compare(\"-\") != 0) {\n\
          \        wrld->_printTo = new std::ofstream (printFile);\n\
          \        if (wrld->_printTo->fail()) {\n\
          \            std::cerr << \"Error opening print file\" << std::endl;\n\
          \            delete wrld;\n\
          \            exit(1);\n\
          \        }\n\
          \    }\n\
          \\n\
          \  // initialize scheduler stuff\n\
          \    if (wrld->_verbose) {\n\
          \        std::cerr << \"initializing world ...\" << std::endl;\n\
          \    }\n\
          \    if (wrld->init()) {\n\
          \        std::cerr << \"Error initializing world:\\n\" << wrld->get_errors() << std::endl;\n\
          \        delete wrld;\n\
          \        exit(1);\n\
          \    }\n\
          \\n\
          \#ifndef DIDEROT_NO_INPUTS\n\
          \  // initialize the input globals\n\
          \    if (init_inputs (wrld, &inputs)) {\n\
          \        std::cerr << \"Error initializing inputs:\\n\" << wrld->get_errors() << std::endl;\n\
          \        delete wrld;\n\
          \        exit(1);\n\
          \    }\n\
          \#endif\n\
          \\n\
          \  // run the generated global initialization code\n\
          \    if (wrld->_verbose) {\n\
          \        std::cerr << \"initializing globals and creating strands ...\\n\";\n\
          \    }\n\
          \    if (wrld->create_strands()) {\n\
          \        std::cerr << \"Error in global initialization:\\n\"\n\
          \            << wrld->get_errors() << std::endl;\n\
          \        delete wrld;\n\
          \        exit(1);\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_EXEC_SNAPSHOT\n\
          \\n\
          \    if (snapshotPeriod > 0) {\n\
          \     // write initial state as snapshot 0\n\
          \        write_snapshot (wrld, \"-0000\");\n\
          \     // run the program for `snapshotPeriod` steps at a time with a snapshot after each run\n\
          \        while (true) {\n\
          \            uint32_t n, limit;\n\
          \          // determine a step limit for the next run\n\
          \            if (stepLimit > 0) {\n\
          \                if (stepLimit <= nSteps) {\n\
          \                    break;\n\
          \                }\n\
          \                limit = std::min(stepLimit - nSteps, snapshotPeriod);\n\
          \            }\n\
          \            else {\n\
          \                limit = snapshotPeriod;\n\
          \            }\n\
          \          // run the program for upto limit steps\n\
          \            if ((n = wrld->run (limit)) == 0) {\n\
          \                break;\n\
          \            }\n\
          \            nSteps += n;\n\
          \            if ((wrld->_errors->errNum > 0) || (wrld->_strands.num_alive() == 0)) {\n\
          \                break;\n\
          \            }\n\
          \          // write a snapshot with the step count as a suffix\n\
          \            std::string suffix = std::to_string(nSteps);\n\
          \            if (suffix.length() < 4) {\n\
          \                suffix = std::string(\"0000\").substr(0, 4 - suffix.length()) + suffix;\n\
          \            }\n\
          \            suffix = \"-\" + suffix;\n\
          \            write_snapshot (wrld, suffix);\n\
          \        }\n\
          \    }\n\
          \    else {\n\
          \        nSteps = wrld->run (stepLimit);\n\
          \    }\n\
          \\n\
          \#else // !DIDEROT_EXEC_SNAPSHOT\n\
          \\n\
          \    nSteps = wrld->run (stepLimit);\n\
          \\n\
          \#endif // DIDEROT_EXEC_SNAPSHOT\n\
          \\n\
          \    if (wrld->_errors->errNum > 0) {\n\
          \        std::cerr << \"Error during execution:\\n\" << wrld->get_errors() << std::endl;\n\
          \        delete wrld;\n\
          \        exit(1);\n\
          \    }\n\
          \\n\
          \    if ((stepLimit != 0) && (wrld->_strands.num_active() > 0)) {\n\
          \#ifdef DIDEROT_STRAND_ARRAY\n\
          \        if (wrld->_verbose) {\n\
          \            std::cerr << \"Step limit expired; \"\n\
          \                << wrld->_strands.num_active() << \" active strands remaining\" << std::endl;\n\
          \        }\n\
          \#else\n\
          \      // step limit expired, so kill remaining strands\n\
          \        if (wrld->_verbose) {\n\
          \            std::cerr << \"Step limit expired. Killing remaining \"\n\
          \                << wrld->_strands.num_active() << \" active strands\" << std::endl;\n\
          \        }\n\
          \        wrld->kill_all();\n\
          \#endif\n\
          \    }\n\
          \\n\
          \    if (wrld->_verbose) {\n\
          \        std::cerr << \"done: \" << nSteps << \" steps, in \" << wrld->_run_time << \" seconds\";\n\
          \#ifndef DIDEROT_STRAND_ARRAY\n\
          \        std::cerr << \"; \" << wrld->_strands.num_stable() << \" stable strands\" << std::endl;\n\
          \#else\n\
          \        std::cerr << std::endl;\n\
          \#endif\n\
          \    }\n\
          \    else if (timingFlg) {\n\
          \        std::cout << \"usr=\" << wrld->_run_time << std::endl;\n\
          \    }\n\
          \\n\
          \  // output the final strand states\n\
          \    write_output (wrld);\n\
          \\n\
          \    delete wrld;\n\
          \\n\
          \    return 0;\n\
          \\n\
          \} // main\n\
          \/*---------- end seq-main.in ----------*/\n\
          \"

    val seqRunNoBSP = "\
          \/*---------- begin seq-run-nobsp.in ----------*/\n\
          \//! Run the Diderot program (sequential version without BSP semantics)\n\
          \//! \\param max_nsteps the limit on the number of super steps; 0 means unlimited\n\
          \//! \\return the number of steps taken, or 0 on error.\n\
          \uint32_t world::run (uint32_t max_nsteps)\n\
          \{\n\
          \    if (this->_stage == diderot::POST_CREATE) {\n\
          \#ifdef DIDEROT_HAS_GLOBAL_START\n\
          \        this->global_start();\n\
          \#endif\n\
          \        this->_stage = diderot::RUNNING;\n\
          \    }\n\
          \    assert (this->_stage == diderot::RUNNING);\n\
          \\n\
          \#ifndef DIDEROT_NO_GLOBALS\n\
          \    globals *glob = this->_globals;\n\
          \#endif\n\
          \\n\
          \    if (max_nsteps == 0) {\n\
          \        max_nsteps = 0xffffffff;  // essentially unlimited\n\
          \    }\n\
          \\n\
          \    double t0 = airTime();\n\
          \\n\
          \    if (this->_verbose) {\n\
          \        std::cerr << \"run with \" << this->_strands.num_alive() << \" strands ...\" << std::endl;\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \    this->run_start_methods();\n\
          \#endif\n\
          \\n\
          \  // iterate until all strands are stable\n\
          \    uint32_t maxSteps = 0;\n\
          \    for (auto ix = this->_strands.begin_active();\n\
          \         ix != this->_strands.end_active();\n\
          \         )\n\
          \    {\n\
          \        diderot::strand_status sts = this->_strands.status(ix);\n\
          \        uint32_t nSteps = 0;\n\
          \        while ((! sts) && (nSteps < max_nsteps)) {\n\
          \            nSteps++;\n\
          \            sts = this->_strands.strand_update(@UPDATE_ARGS_IN_WRLD@ix);\n\
          \        }\n\
          \        switch (sts) {\n\
          \          case diderot::kStabilize:\n\
          \          // stabilize the strand's state.\n\
          \            ix = this->_strands.strand_stabilize (@STABILIZE_ARGS_IN_WRLD@ix);\n\
          \            break;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \          case diderot::kDie:\n\
          \            ix = this->_strands.kill (ix);\n\
          \            break;\n\
          \#endif\n\
          \          default:\n\
          \            assert (sts == this->_strands.status(ix));\n\
          \\t    ix = this->_strands.next_active(ix);\n\
          \            break;\n\
          \        }\n\
          \        if (maxSteps < nSteps) maxSteps = nSteps;\n\
          \    }\n\
          \\n\
          \    this->_run_time += airTime() - t0;\n\
          \\n\
          \    if (this->_strands.num_active() == 0)\n\
          \        this->_stage = diderot::DONE;\n\
          \\n\
          \    return maxSteps;\n\
          \\n\
          \} // world::run\n\
          \/*---------- end seq-run-nobsp.in ----------*/\n\
          \"

    val seqRun = "\
          \/*---------- begin seq-run.in ----------*/\n\
          \//! Run the Diderot program (sequential version)\n\
          \//! \\param max_nsteps the limit on the number of super steps; 0 means unlimited\n\
          \//! \\return the number of steps taken, or -1 on error.\n\
          \uint32_t world::run (uint32_t max_nsteps)\n\
          \{\n\
          \    if (this->_stage == diderot::POST_CREATE) {\n\
          \#ifdef DIDEROT_HAS_GLOBAL_START\n\
          \        this->global_start();\n\
          \#endif\n\
          \        this->_stage = diderot::RUNNING;\n\
          \    }\n\
          \    assert (this->_stage == diderot::RUNNING);\n\
          \\n\
          \#ifndef DIDEROT_NO_GLOBALS\n\
          \    globals *glob = this->_globals;\n\
          \#endif\n\
          \\n\
          \    if (max_nsteps == 0) {\n\
          \        max_nsteps = 0xffffffff;  // essentially unlimited\n\
          \    }\n\
          \\n\
          \    double t0 = airTime();\n\
          \\n\
          \    if (this->_verbose) {\n\
          \        std::cerr << \"run with \" << this->_strands.num_active() << \" strands ...\" << std::endl;\n\
          \    }\n\
          \\n\
          \#if defined(DIDEROT_HAS_STRAND_COMMUNICATION) && !(defined(DIDEROT_HAS_STRAND_DIE) || defined(DIDEROT_HAS_STRAND_NEW))\n\
          \  // initial recording of strands for KD-tree\n\
          \    this->_tree->update_strands ();\n\
          \#endif\n\
          \\n\
          \  // iterate until all strands are stable\n\
          \    bool treeNeedsUpdate = true;\n\
          \    uint32_t nSteps = 0;\n\
          \    while ((this->_strands.num_active() > 0) && (nSteps < max_nsteps)) {\n\
          \        nSteps++;\n\
          \#ifdef DIDEROT_HAS_STRAND_COMMUNICATION\n\
          \      // build spatial partition to support communication\n\
          \        if (treeNeedsUpdate) {\n\
          \\t    this->_tree->update_strands ();\n\
          \        }\n\
          \        this->_tree->rebuild ();\n\
          \#endif\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \      // run start methods for fresh strands\n\
          \        this->run_start_methods();\n\
          \#endif\n\
          \      // update strands\n\
          \        for (auto ix = this->_strands.begin_active();\n\
          \            ix != this->_strands.end_active();\n\
          \            )\n\
          \        {\n\
          \            diderot::strand_status sts = this->_strands.strand_update (@UPDATE_ARGS_IN_WRLD@ix);\n\
          \            switch (sts) {\n\
          \              case diderot::kStabilize:\n\
          \                ix = this->_strands.strand_stabilize(@STABILIZE_ARGS_IN_WRLD@ix);\n\
          \                break;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \              case diderot::kDie:\n\
          \                ix = this->_strands.kill(ix);\n\
          \                break;\n\
          \#endif\n\
          \              default:\n\
          \                ix = this->_strands.next_active(ix);\n\
          \                break;\n\
          \            }\n\
          \        }\n\
          \      // finish the local-phase of the superstep by updating strand status\n\
          \        treeNeedsUpdate = this->_strands.finish_step();\n\
          \\n\
          \        this->swap_state();\n\
          \\n\
          \#ifdef DIDEROT_HAS_GLOBAL_UPDATE\n\
          \        this->global_update();\n\
          \#endif\n\
          \    }\n\
          \\n\
          \    this->_run_time += airTime() - t0;\n\
          \\n\
          \    if (this->_strands.num_active() == 0)\n\
          \        this->_stage = diderot::DONE;\n\
          \\n\
          \    return nSteps;\n\
          \\n\
          \} // world::run\n\
          \/*---------- end seq-run.in ----------*/\n\
          \"

    val seqRunStartMethods = "\
          \/*---------- begin seq-run-start.in ----------*/\n\
          \// Run the start methods of the initial strands (sequential version)\n\
          \//\n\
          \void world::run_start_methods ()\n\
          \{\n\
          \#ifndef DIDEROT_NO_GLOBALS\n\
          \    globals *glob = this->_globals;\n\
          \#endif\n\
          \\n\
          \    for (auto ix = this->_strands.begin_fresh();\n\
          \        ix != this->_strands.end_fresh();\n\
          \        )\n\
          \    {\n\
          \        diderot::strand_status sts = this->_strands.strand_start(@START_ARGS_IN_WRLD@ix);\n\
          \        switch (sts) {\n\
          \          case diderot::kStabilize:\n\
          \            ix = this->_strands.strand_stabilize (@STABILIZE_ARGS_IN_WRLD@ix);\n\
          \            break;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \          case diderot::kDie:\n\
          \            ix = this->_strands.kill (ix);\n\
          \            break;\n\
          \#endif\n\
          \          default:\n\
          \            assert (sts == this->_strands.status(ix));\n\
          \            ix = this->_strands.next_fresh(ix);\n\
          \            break;\n\
          \        }\n\
          \    }\n\
          \    this->_strands._nFresh = 0;\n\
          \\n\
          \}\n\
          \/*---------- end seq-run-start.in ----------*/\n\
          \"

    val seqSArrayDualInd = "\
          \/*---------- begin seq-sarr-dual-indirect.in ----------*/\n\
          \// forward declarations of strand methods\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_start (@START_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \static void @STRAND@_stabilize (@STABILIZE_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \\n\
          \// strand_array for BSP/DUAL STATE/INDIRECT ACCESS\n\
          \//\n\
          \struct strand_array {\n\
          \    typedef @STRANDTY@ strand_t;\n\
          \    typedef uint32_t index_t;           // strand index (index into _idx and _status arrays)\n\
          \    typedef uint32_t sid_t;             // strand ID (index into strand-state storage)\n\
          \    typedef char *block_t;              // points to array of @STRANDTY@ structs\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    uint32_t            *_idx;          // array of strand indices for indirect state rep.\n\
          \    std::vector<block_t> _blocks;       // vector of pointers to strand-storage blocks\n\
          \    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)\n\
          \    uint32_t            _nItems;        // number of items in the _blocks and _status arrays\n\
          \    uint32_t            _nStable;       // stable strands (in locations 0.._nStable-1)\n\
          \    uint32_t            _nActive;       // active strands (in locations _nStable.._nStable+_nActive-1)\n\
          \    uint32_t            _nStabilizing;  // number of stablizing strands\n\
          \    uint32_t            _nDying;        // number of dying strands\n\
          \    uint32_t            _nNew;          // number of new strands\n\
          \    uint32_t            _nFresh;        // number of fresh strands (new strands from previous step)\n\
          \\n\
          \    static const uint32_t LOG_BLKSZ = 12;               // 2^12 items per block\n\
          \    static const uint32_t BLKSZ = (1 << LOG_BLKSZ);\n\
          \    static const uint32_t BLKMASK = (BLKSZ-1);          // mask for block index\n\
          \\n\
          \    strand_array () : _status(nullptr), _idx(nullptr), _nItems(0) { }\n\
          \    ~strand_array ();\n\
          \\n\
          \    uint32_t in_state_index () const { return this->_inIdx; }\n\
          \\n\
          \    uint32_t num_active () const { return this->_nActive; }\n\
          \    uint32_t num_stable () const { return this->_nStable; }\n\
          \    uint32_t num_alive () const { return this->_nActive+this->_nStable; }\n\
          \    uint32_t num_fresh () const { return this->_nFresh; }\n\
          \\n\
          \  // return the ID of a strand, which is the value of the _idx array\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return this->_idx[ix];\n\
          \    }\n\
          \  // return a pointer to the strand with the given ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        assert (id < this->_nItems);\n\
          \        uint32_t blkId = id >> LOG_BLKSZ;\n\
          \        uint32_t offset = id & BLKMASK;\n\
          \        return reinterpret_cast<@STRANDTY@ *>(this->_blocks[blkId] + offset * sizeof(@STRANDTY@));\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRAND@_local *local_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_local);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRAND@_local *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return &(this->id_to_strand(id)->_local);\n\
          \    }\n\
          \  // return a pointer to the in-state of strand ix\n\
          \    const @STRAND@_shared *in_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_shared[this->_inIdx]);\n\
          \    }\n\
          \  // return a pointer to the in-state of the strand with the given ID\n\
          \    const @STRAND@_shared *id_to_in_state (sid_t id) const\n\
          \    {\n\
          \        return &(this->id_to_strand(id)->_shared[this->_inIdx]);\n\
          \    }\n\
          \  // return a pointer to the out-state of strand ix\n\
          \    @STRAND@_shared *out_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_shared[this->_inIdx ^ 1]);\n\
          \    }\n\
          \\n\
          \  // wrappers for accessing the state of newly created strands\n\
          \    @STRAND@_local *new_local_state (index_t ix) const\n\
          \    {\n\
          \        return this->local_state(ix);\n\
          \    }\n\
          \    @STRAND@_shared *new_out_state (index_t ix) const\n\
          \    {\n\
          \        return this->out_state(ix);\n\
          \    }\n\
          \\n\
          \  // is an index valid for the strand array?\n\
          \    bool validIndex (index_t ix) const { return (ix < this->_nItems); }\n\
          \\n\
          \  // is a given strand alive?\n\
          \    bool isAlive (index_t ix) const\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        return aliveSts(this->status(ix));\n\
          \#else\n\
          \        return true;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \  // deallocate space reserved for strands\n\
          \    void dealloc ();\n\
          \\n\
          \  // allocate space for at least nItems\n\
          \    bool alloc (uint32_t nItems)\n\
          \    {\n\
          \        nItems = (nItems + BLKSZ - 1) & ~BLKMASK;\n\
          \        uint32_t nBlks = nItems >> LOG_BLKSZ;\n\
          \      // allocate block vector\n\
          \        this->_blocks.resize(nBlks, nullptr);\n\
          \      // allocate blocks\n\
          \        for (int i = 0;  i < nBlks;  i++) {\n\
          \            this->_blocks[i] = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));\n\
          \            if (this->_blocks[i]  == nullptr) {\n\
          \              // unable to allocate memory\n\
          \                this->dealloc();\n\
          \                return true;\n\
          \            }\n\
          \        }\n\
          \      // allocate _status array\n\
          \        this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));\n\
          \        if (this->_status == nullptr) {\n\
          \            this->dealloc();\n\
          \            return true;\n\
          \        }\n\
          \      // allocate _idx array\n\
          \        this->_idx = static_cast<uint32_t *>(std::malloc (nItems * sizeof(uint32_t)));\n\
          \        if (this->_idx == nullptr) {\n\
          \            this->dealloc();\n\
          \            return true;\n\
          \        }\n\
          \      // initialize arrays\n\
          \        for (index_t ix = 0;  ix < nItems;  ix++) {\n\
          \            this->_status[ix] = diderot::kDead;\n\
          \            this->_idx[ix] = ix;\n\
          \        }\n\
          \        this->_inIdx = 0;\n\
          \        this->_nItems = nItems;\n\
          \        this->_nActive = 0;\n\
          \        this->_nStable = 0;\n\
          \        this->_nStabilizing = 0;\n\
          \        this->_nNew = 0;\n\
          \        this->_nDying = 0;\n\
          \        this->_nFresh = 0;\n\
          \        return false;\n\
          \    }\n\
          \\n\
          \  // initialize the first nStrands locations as new active strands\n\
          \    void create_strands (uint32_t nStrands)\n\
          \    {\n\
          \        assert (this->_nActive == 0);\n\
          \        assert (this->_nItems >= nStrands);\n\
          \        for (index_t ix = 0;  ix < nStrands;  ix++) {\n\
          \            this->_status[ix] = diderot::kActive;\n\
          \            new (this->strand(ix)) @STRANDTY@;\n\
          \        }\n\
          \        this->_nActive = nStrands;\n\
          \        this->_nFresh = nStrands;\n\
          \    }\n\
          \\n\
          \  // swap in and out states\n\
          \    void swap ()\n\
          \    {\n\
          \        this->_inIdx ^= 1;\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \  // invoke strand's start method\n\
          \    diderot::strand_status strand_start (@START_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        return @STRAND@_start (@START_ARGS@\n\
          \            &self->_local,\n\
          \            &self->_shared[this->_inIdx],\n\
          \            &self->_shared[this->_inIdx^1]);\n\
          \    }\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \\n\
          \  // invoke strand's update method\n\
          \    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        return @STRAND@_update (@UPDATE_ARGS@\n\
          \            &self->_local,\n\
          \            &self->_shared[this->_inIdx],\n\
          \            &self->_shared[this->_inIdx^1]);\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method\n\
          \    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];\n\
          \        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // note that we swap out and in here because out holds the current state\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);\n\
          \        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));\n\
          \#else\n\
          \        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // we swap the strand-indices at ix and _nStable + this->_nStabilizing\n\
          \        uint32_t jx = this->_nStable + this->_nStabilizing;\n\
          \        this->_status[jx] = diderot::kStabilize;\n\
          \        std::swap (this->_idx[ix], this->_idx[jx]);\n\
          \        this->_nStabilizing++;\n\
          \        return ix+1;\n\
          \    }\n\
          \\n\
          \  // record that the specified strand is dying\n\
          \    index_t kill (index_t ix)\n\
          \    {\n\
          \        assert (this->_nStable <= ix);\n\
          \        assert (ix < this->num_alive());\n\
          \        this->_nDying++;\n\
          \        uint32_t jx = this->num_alive() - this->_nDying;\n\
          \        this->_status[jx] = diderot::kDie;\n\
          \        std::swap (this->_idx[ix], this->_idx[jx]);\n\
          \        return ix;  // don't advance, since ix is an active strand after the swap\n\
          \    }\n\
          \\n\
          \  // allocate a new strand\n\
          \    index_t new_strand ()\n\
          \    {\n\
          \        index_t ix = this->num_alive() + this->_nNew;\n\
          \        if (this->_nItems <= ix) {\n\
          \            if (this->grow ()) {\n\
          \                std::cerr << \"Fatal error: unable to allocate space for new strands\" << std::endl;\n\
          \                exit (1);\n\
          \            }\n\
          \        }\n\
          \        this->_status[ix] = diderot::kNew;\n\
          \        new (this->strand(ix)) @STRANDTY@;\n\
          \        this->_nNew++;\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // finish a step by updating the strand statuses and the various counters\n\
          \    bool finish_step ()\n\
          \    {\n\
          \        bool anyNewDie = ((this->_nDying + this->_nNew) > 0);\n\
          \        index_t next = this->_nStable;\n\
          \        for (index_t ix = 0;  ix < this->_nStabilizing;  ix++, next++) {\n\
          \            this->_status[next] = diderot::kStable;\n\
          \        }\n\
          \        if (this->_nDying == 0) {\n\
          \          // no need to swap strands\n\
          \            index_t next = this->num_alive();\n\
          \            for (auto ix = 0;  ix < this->_nNew;  ix++, next++) {\n\
          \                this->_status[next] = diderot::kActive;\n\
          \            }\n\
          \        }\n\
          \        else {\n\
          \          // first handle the dying strands\n\
          \            next = this->num_alive() - this->_nDying;\n\
          \            for (index_t ix = 0;  ix < this->_nDying;  ix++, next++) {\n\
          \                this->_status[next] = diderot::kDead;\n\
          \              // invoke the dead strand's destructors\n\
          \                reinterpret_cast<@STRANDTY@ *>(this->strand(next))->~@STRANDTY@();\n\
          \            }\n\
          \          // move the new strands down over the dying strands\n\
          \            index_t src = this->num_alive();\n\
          \            index_t dst = src - this->_nDying;\n\
          \            for (auto ix = 0;  ix < this->_nNew;  ix++, dst++, src++) {\n\
          \                this->_status[dst] = diderot::kActive;\n\
          \                this->_status[src] = diderot::kDead;\n\
          \                std::swap (this->_idx[src], this->_idx[dst]);\n\
          \            }\n\
          \        }\n\
          \\n\
          \      // update counts\n\
          \        this->_nFresh = this->_nNew;\n\
          \        this->_nStable += this->_nStabilizing;\n\
          \        this->_nActive -= this->_nStabilizing + this->_nDying;\n\
          \        this->_nActive += this->_nNew;\n\
          \        this->_nStabilizing = 0;\n\
          \        this->_nNew = 0;\n\
          \        this->_nDying = 0;\n\
          \\n\
          \        return anyNewDie;\n\
          \    }\n\
          \\n\
          \  // finish a kill_all operation\n\
          \    void finish_kill_all ()\n\
          \    {\n\
          \        this->_nActive -= this->_nDying;\n\
          \        this->_nDying = 0;\n\
          \    }\n\
          \\n\
          \  // finish a stabilize_all operation\n\
          \    void finish_stabilize_all ()\n\
          \    {\n\
          \        this->_nStable += this->_nStabilizing;\n\
          \        this->_nActive -= this->_nStabilizing;\n\
          \        this->_nStabilizing = 0;\n\
          \    }\n\
          \\n\
          \  // iterator over stable strands\n\
          \    index_t begin_stable () const { return 0; }\n\
          \    index_t end_stable () const { return this->_nStable; }\n\
          \    index_t next_stable (index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // iterator over active strands\n\
          \    index_t begin_active () const { return this->_nStable+this->_nStabilizing; }\n\
          \    index_t end_active () const { return this->_nStable+this->_nActive-this->_nDying; }\n\
          \    index_t next_active (index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // iterator over alive (active+stable) strands; we assume that _nStabilizing and _nNew are 0\n\
          \    index_t begin_alive () const { return 0; }\n\
          \    index_t end_alive () const { return this->num_alive(); }\n\
          \    index_t next_alive (index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // iterator over fresh strands\n\
          \    index_t begin_fresh () const { return this->num_alive() - this->_nFresh; }\n\
          \    index_t end_fresh () const { return this->num_alive(); }\n\
          \    index_t next_fresh (index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // allocate more space for strand state; return true on error\n\
          \    bool grow ()\n\
          \    {\n\
          \        size_t nItems = static_cast<size_t>(this->_nItems) + BLKSZ;\n\
          \        if (nItems >= UINT32_MAX) {\n\
          \          // cannot have more than UINT32_MAX elements\n\
          \            return true;\n\
          \        }\n\
          \\n\
          \      // allocate a new block at the end of the _blocks array\n\
          \        char *blk = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));\n\
          \        if (blk == nullptr) {\n\
          \            return true;\n\
          \        }\n\
          \        this->_blocks.push_back (blk);\n\
          \\n\
          \      // grow the _status and _idx arrays\n\
          \        uint8_t *status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));\n\
          \        uint32_t *idx = static_cast<uint32_t *>(std::malloc (nItems * sizeof(uint32_t)));\n\
          \        if ((status == nullptr) || (idx == nullptr)) {\n\
          \            return true;\n\
          \        }\n\
          \        std::memcpy (status, this->_status, this->_nItems * sizeof(uint8_t));\n\
          \        std::memcpy (idx, this->_idx, this->_nItems * sizeof(uint32_t));\n\
          \\n\
          \      // initialize the new storage\n\
          \        @STRANDTY@ *p = reinterpret_cast<@STRANDTY@ *>(blk);\n\
          \        for (uint32_t ix = this->_nItems;  ix < nItems;  ix++) {\n\
          \            status[ix] = diderot::kDead;\n\
          \            idx[ix] = ix;\n\
          \        }\n\
          \\n\
          \      // free the old storage\n\
          \        std::free (this->_status);\n\
          \        std::free (this->_idx);\n\
          \\n\
          \      // update pointers\n\
          \        this->_status = status;\n\
          \        this->_idx = idx;\n\
          \        this->_nItems = nItems;\n\
          \\n\
          \        return false;\n\
          \    }\n\
          \\n\
          \}; // struct strand_array\n\
          \\n\
          \strand_array::~strand_array ()\n\
          \{\n\
          \  // run destructors to reclaim any dynamic memory attached to the strand state\n\
          \    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {\n\
          \        this->strand(ix)->~@STRANDTY@();\n\
          \    }\n\
          \    this->dealloc();\n\
          \}\n\
          \\n\
          \void strand_array::dealloc ()\n\
          \{\n\
          \    if (this->_status != nullptr) {\n\
          \        std::free (this->_status);\n\
          \        this->_status = nullptr;\n\
          \    }\n\
          \    if (this->_idx != nullptr) {\n\
          \        std::free (this->_idx);\n\
          \        this->_idx = nullptr;\n\
          \    }\n\
          \    for (uint32_t i = 0;  i < this->_blocks.size();  i++) {\n\
          \        if (this->_blocks[i] != nullptr) {\n\
          \            std::free (this->_blocks[i]);\n\
          \            this->_blocks[i] = nullptr;\n\
          \        }\n\
          \        else {\n\
          \            break;\n\
          \        }\n\
          \    }\n\
          \}\n\
          \/*---------- end seq-sarr-dual-indirect.in ----------*/\n\
          \"

    val seqSArrayDualDir = "\
          \/*---------- begin seq-sarr-dual.in ----------*/\n\
          \// forward declarations of strand methods\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_start (@START_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \static void @STRAND@_stabilize (@STABILIZE_PARAMS@\n\
          \    @STRAND@_local *selfLocal, @STRAND@_shared *selfIn, @STRAND@_shared *selfOut);\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \\n\
          \// if we have both communication and \"die\", then we need to track when strands die\n\
          \// so that we can rebuild the list of strands use to construct the kd-tree\n\
          \#if defined(DIDEROT_HAS_STRAND_COMMUNICATION) && !defined(DIDEROT_HAS_STRAND_DIE)\n\
          \#  define TRACK_STRAND_DEATH\n\
          \#endif\n\
          \\n\
          \// strand_array for SEQUENTIAL/BSP/DUAL STATE/DIRECT ACCESS\n\
          \//\n\
          \struct strand_array {\n\
          \    typedef @STRANDTY@ strand_t;\n\
          \    typedef uint32_t index_t;\n\
          \    typedef index_t sid_t;              // strand ID (index into strand-state storage)\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    char                *_storage;      // points to array of @STRANDTY@ structs\n\
          \    uint32_t            _inIdx;         // index of shared input state (either 0 or 1)\n\
          \    uint32_t            _nItems;        // number of items in the _storage and _status arrays\n\
          \    uint32_t            _nStable;       // number of stable strands\n\
          \    uint32_t            _nActive;       // number of active strands\n\
          \    uint32_t            _nFresh;        // number of fresh strands (new strands from create_strands)\n\
          \#ifdef TRACK_STRAND_DEATH\n\
          \    bool                _died;          // a strand died in the current superstep.\n\
          \#endif\n\
          \\n\
          \    strand_array () : _status(nullptr), _storage(nullptr), _nItems(0) { }\n\
          \    ~strand_array ();\n\
          \\n\
          \    uint32_t in_state_index () const { return this->_inIdx; }\n\
          \\n\
          \    uint32_t num_active () const { return this->_nActive; }\n\
          \    uint32_t num_stable () const { return this->_nStable; }\n\
          \    uint32_t num_alive () const { return this->_nActive+this->_nStable; }\n\
          \\n\
          \  // return the ID of a strand, which is the same as the ix index\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return ix;\n\
          \    }\n\
          \  // return a pointer to the strand with the given ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        assert (id < this->_nItems);\n\
          \        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRAND@_local *local_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_local);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRAND@_local *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return &(this->id_to_strand(id)->_local);\n\
          \    }\n\
          \  // return a pointer to the in-state of strand ix\n\
          \    const @STRAND@_shared *in_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_shared[this->_inIdx]);\n\
          \    }\n\
          \  // return a pointer to the in-state of the strand with the given ID\n\
          \    const @STRAND@_shared *id_to_in_state (sid_t id) const\n\
          \    {\n\
          \        return &(this->id_to_strand(id)->_shared[this->_inIdx]);\n\
          \    }\n\
          \  // return a pointer to the out-state of strand ix\n\
          \    @STRAND@_shared *out_state (index_t ix) const\n\
          \    {\n\
          \        return &(this->strand(ix)->_shared[this->_inIdx ^ 1]);\n\
          \    }\n\
          \\n\
          \  // is an index valid for the strand array?\n\
          \    bool validIndex (index_t ix) const { return (ix < this->_nItems); }\n\
          \\n\
          \  // is a given strand alive?\n\
          \    bool isAlive (index_t ix) const\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        return aliveSts(this->status(ix));\n\
          \#else\n\
          \        return true;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \  // allocate space for nItems\n\
          \    bool alloc (uint32_t nItems)\n\
          \    {\n\
          \        this->_storage = static_cast<char *>(std::malloc (nItems * sizeof(@STRANDTY@)));\n\
          \        if (this->_storage == nullptr) {\n\
          \            return true;\n\
          \        }\n\
          \        this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));\n\
          \        if (this->_status == nullptr) {\n\
          \            std::free (this->_storage);\n\
          \            return true;\n\
          \        }\n\
          \        this->_inIdx = 0;\n\
          \        this->_nItems = nItems;\n\
          \        this->_nActive = 0;\n\
          \        this->_nStable = 0;\n\
          \        this->_nFresh = 0;\n\
          \        return false;\n\
          \    }\n\
          \\n\
          \  // initialize the first nStrands locations as new active strands\n\
          \    void create_strands (uint32_t nStrands)\n\
          \    {\n\
          \        assert (this->_nActive == 0);\n\
          \        assert (this->_nItems == nStrands);\n\
          \        for (index_t ix = 0;  ix < nStrands;  ix++) {\n\
          \            this->_status[ix] = diderot::kActive;\n\
          \            new (this->strand(ix)) @STRANDTY@;\n\
          \        }\n\
          \        this->_nActive = nStrands;\n\
          \        this->_nFresh = nStrands;\n\
          \#ifdef TRACK_STRAND_DEATH\n\
          \        this->_died = false;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \  // swap in and out states\n\
          \    void swap ()\n\
          \    {\n\
          \        this->_inIdx ^= 1;\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \  // invoke strand's start method\n\
          \    diderot::strand_status strand_start (@START_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        return @STRAND@_start (@START_ARGS@\n\
          \            &self->_local,\n\
          \            &self->_shared[this->_inIdx],\n\
          \            &self->_shared[this->_inIdx^1]);\n\
          \    }\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \\n\
          \  // invoke strand's update method\n\
          \    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        return @STRAND@_update (@UPDATE_ARGS@\n\
          \            &self->_local,\n\
          \            &self->_shared[this->_inIdx],\n\
          \            &self->_shared[this->_inIdx^1]);\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method\n\
          \    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \        @STRANDTY@ *self = this->strand(ix);\n\
          \        @STRAND@_shared *selfIn = &self->_shared[this->_inIdx];\n\
          \        @STRAND@_shared *selfOut = &self->_shared[this->_inIdx^1];\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // note that we swap out and in here because out holds the current state\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@&self->_local, selfOut, selfIn);\n\
          \        std::memcpy (selfOut, selfIn, sizeof(@STRAND@_shared));\n\
          \#else\n\
          \        std::memcpy (selfIn, selfOut, sizeof(@STRAND@_shared));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \        this->_status[ix] = diderot::kStable;\n\
          \        this->_nActive--;\n\
          \        this->_nStable++;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // mark the given strand as dead\n\
          \    index_t kill (index_t ix)\n\
          \    {\n\
          \#ifdef TRACK_STRAND_DEATH\n\
          \        this->_died = true;\n\
          \#endif\n\
          \        this->_status[ix] = diderot::kDead;\n\
          \        this->_nActive--;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // finish the local-phase of a superstep (NOP)\n\
          \#ifdef TRACK_STRAND_DEATH\n\
          \    bool finish_step ()\n\
          \    {\n\
          \        bool res = this->_died;\n\
          \        this->_died = false;\n\
          \        return res;\n\
          \    }\n\
          \#else\n\
          \    bool finish_step () { return false; }\n\
          \#endif\n\
          \\n\
          \  // finish a kill_all operation (NOP)\n\
          \    void finish_kill_all () { }\n\
          \\n\
          \  // finish a stabilize_all operation (NOP)\n\
          \    void finish_stabilize_all () { }\n\
          \\n\
          \    index_t begin_alive () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_alive () const { return this->_nItems; }\n\
          \    index_t next_alive (index_t &ix) const\n\
          \    {\n\
          \        ix++;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over active strands\n\
          \    index_t begin_active () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nItems) && notActiveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_active () const { return this->_nItems; }\n\
          \    index_t next_active (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over stable strands\n\
          \    index_t begin_stable () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable)) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_stable () const { return this->_nItems; }\n\
          \    index_t next_stable (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over fresh strands; since the only new strands were created by create_strand\n\
          \  // we iterate over all of them\n\
          \    index_t begin_fresh () const { return 0; }\n\
          \    index_t end_fresh () const { return this->_nFresh; }\n\
          \    index_t next_fresh (index_t &ix) const { return ++ix; }\n\
          \\n\
          \}; // struct strand_array\n\
          \\n\
          \strand_array::~strand_array ()\n\
          \{\n\
          \  // run destructors to reclaim any dynamic memory attached to the strand state\n\
          \    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {\n\
          \        this->strand(ix)->~@STRANDTY@();\n\
          \    }\n\
          \    if (this->_status != nullptr) std::free (this->_status);\n\
          \    if (this->_storage != nullptr) std::free (this->_storage);\n\
          \}\n\
          \/*---------- end seq-sarr-dual.in ----------*/\n\
          \"

    val seqSArrayInd = "\
          \/*---------- begin seq-sarr-indirect.in ----------*/\n\
          \// forward declarations of strand methods\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_start (@START_PARAMS@@STRANDTY@ *self);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@@STRANDTY@ *self);\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \static void @STRAND@_stabilize (@STABILIZE_PARAMS@@STRANDTY@ *self);\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \\n\
          \// strand_array for BSP/SINGLE STATE/INDIRECT ACCESS\n\
          \//\n\
          \struct strand_array {\n\
          \    typedef @STRANDTY@ strand_t;\n\
          \    typedef uint32_t index_t;           // strand index (index into _idx and _status arrays)\n\
          \    typedef uint32_t sid_t;             // strand ID (index into strand-state storage)\n\
          \    typedef char *block_t;              // points to array of @STRANDTY@ structs\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    uint32_t            *_idx;          // array of strand indices for indirect state rep.\n\
          \    std::vector<block_t> _blocks;       // vector of pointers to strand-storage blocks\n\
          \    uint32_t            _nItems;        // number of items in the _blocks and _status arrays\n\
          \    uint32_t            _nStable;       // stable strands (in locations 0.._nStable-1)\n\
          \    uint32_t            _nActive;       // active strands (in locations _nStable.._nStable+_nActive-1)\n\
          \    uint32_t            _nStabilizing;  // number of stablizing strands\n\
          \    uint32_t            _nDying;        // number of dying strands\n\
          \    uint32_t            _nNew;          // number of new strands\n\
          \    uint32_t            _nFresh;        // number of fresh strands (new strands from previous step)\n\
          \\n\
          \  // size info for block_t objects\n\
          \    static const uint32_t LOG_BLKSZ = 12;               // 2^12 items per block\n\
          \    static const uint32_t BLKSZ = (1 << LOG_BLKSZ);\n\
          \    static const uint32_t BLKMASK = (BLKSZ-1);          // mask for block index\n\
          \\n\
          \    strand_array () : _status(nullptr), _idx(nullptr), _nItems(0) { }\n\
          \    ~strand_array ();\n\
          \\n\
          \    uint32_t in_state_index () const { return 0; /* dummy */ }\n\
          \\n\
          \    uint32_t num_active () const { return this->_nActive; }\n\
          \    uint32_t num_stable () const { return this->_nStable; }\n\
          \    uint32_t num_alive () const { return this->_nActive+this->_nStable; }\n\
          \    uint32_t num_fresh () const { return this->_nFresh; }\n\
          \\n\
          \  // return the ID of a strand, which is the value of the _idx array\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return this->_idx[ix];\n\
          \    }\n\
          \  // direct indexing of strands by ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        assert (id < this->_nItems);\n\
          \        uint32_t blkId = id >> LOG_BLKSZ;\n\
          \        uint32_t offset = id & BLKMASK;\n\
          \        return reinterpret_cast<@STRANDTY@ *>(this->_blocks[blkId] + offset * sizeof(@STRANDTY@));\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRANDTY@ *local_state (index_t ix) const\n\
          \    {\n\
          \        return this->strand(ix);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRANDTY@ *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return this->id_to_strand(id);\n\
          \    }\n\
          \\n\
          \  // wrappers for accessing the state of newly created strands\n\
          \    @STRANDTY@ *new_strand_state (index_t ix) const\n\
          \    {\n\
          \        return this->strand(ix);\n\
          \    }\n\
          \\n\
          \  // is an index valid for the strand array?\n\
          \    bool validIndex (index_t ix) const { return (ix < this->_nItems); }\n\
          \\n\
          \  // is a given strand alive?\n\
          \    bool isAlive (index_t ix) const\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        return aliveSts(this->status(ix));\n\
          \#else\n\
          \        return true;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \  // deallocate space reserved for strands\n\
          \    void dealloc ();\n\
          \\n\
          \  // allocate space for at least nItems\n\
          \    bool alloc (uint32_t nItems)\n\
          \    {\n\
          \        nItems = (nItems + BLKSZ - 1) & ~BLKMASK;\n\
          \        uint32_t nBlks = nItems >> LOG_BLKSZ;\n\
          \        assert (nItems == nBlks*BLKSZ);\n\
          \      // allocate block vector\n\
          \        this->_blocks.resize(nBlks, nullptr);\n\
          \      // allocate blocks\n\
          \        for (int i = 0;  i < nBlks;  i++) {\n\
          \            this->_blocks[i] = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));\n\
          \            if (this->_blocks[i]  == nullptr) {\n\
          \              // unable to allocate memory\n\
          \                this->dealloc();\n\
          \                return true;\n\
          \            }\n\
          \        }\n\
          \      // allocate _status array\n\
          \        this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));\n\
          \        if (this->_status == nullptr) {\n\
          \            this->dealloc();\n\
          \            return true;\n\
          \        }\n\
          \      // allocate _idx array\n\
          \        this->_idx = static_cast<uint32_t *>(std::malloc (nItems * sizeof(uint32_t)));\n\
          \        if (this->_idx == nullptr) {\n\
          \            this->dealloc();\n\
          \            return true;\n\
          \        }\n\
          \      // initialize arrays\n\
          \        for (index_t ix = 0;  ix < nItems;  ix++) {\n\
          \            this->_status[ix] = diderot::kDead;\n\
          \            this->_idx[ix] = ix;\n\
          \        }\n\
          \        this->_nItems = nItems;\n\
          \        this->_nActive = 0;\n\
          \        this->_nStable = 0;\n\
          \        this->_nStabilizing = 0;\n\
          \        this->_nNew = 0;\n\
          \        this->_nDying = 0;\n\
          \        this->_nFresh = 0;\n\
          \        return false;\n\
          \    }\n\
          \\n\
          \  // initialize the first nStrands locations as new active strands\n\
          \    void create_strands (uint32_t nStrands)\n\
          \    {\n\
          \        assert (this->_nActive == 0);\n\
          \        assert (this->_nItems >= nStrands);\n\
          \        for (index_t ix = 0;  ix < nStrands;  ix++) {\n\
          \            this->_status[ix] = diderot::kActive;\n\
          \            new (this->strand(ix)) @STRANDTY@;\n\
          \        }\n\
          \        this->_nActive = nStrands;\n\
          \        this->_nFresh = nStrands;\n\
          \    }\n\
          \\n\
          \  // swap in and out states (NOP for this version)\n\
          \    void swap () { }\n\
          \\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \  // invoke strand's start method\n\
          \    diderot::strand_status strand_start (@START_PARAMS@index_t ix)\n\
          \    {\n\
          \        return @STRAND@_start(@START_ARGS@this->strand(ix));\n\
          \    }\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \\n\
          \  // invoke strand's update method\n\
          \    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)\n\
          \    {\n\
          \        return @STRAND@_update(@UPDATE_ARGS@this->strand(ix));\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method\n\
          \    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \      // we swap the strand-indices at ix and _nStable + this->_nStabilizing\n\
          \        uint32_t jx = this->_nStable + this->_nStabilizing;\n\
          \        this->_status[jx] = diderot::kStabilize;\n\
          \        std::swap (this->_idx[ix], this->_idx[jx]);\n\
          \        this->_nStabilizing++;\n\
          \        return ix+1;\n\
          \    }\n\
          \\n\
          \  // record that the specified strand is dying\n\
          \    index_t kill (index_t ix)\n\
          \    {\n\
          \        assert (this->_nStable <= ix);\n\
          \        assert (ix < this->num_alive());\n\
          \        this->_nDying++;\n\
          \        uint32_t jx = this->num_alive() - this->_nDying;\n\
          \        this->_status[jx] = diderot::kDie;\n\
          \        std::swap (this->_idx[ix], this->_idx[jx]);\n\
          \        return ix;  // don't advance, since ix is an active strand after the swap\n\
          \    }\n\
          \\n\
          \  // allocate a new strand\n\
          \    index_t new_strand ()\n\
          \    {\n\
          \        index_t ix = this->num_alive() + this->_nNew;\n\
          \        if (this->_nItems <= ix) {\n\
          \            if (this->grow ()) {\n\
          \                std::cerr << \"Fatal error: unable to allocate space for new strands\" << std::endl;\n\
          \                exit (1);\n\
          \            }\n\
          \        }\n\
          \        this->_status[ix] = diderot::kNew;\n\
          \        new (this->strand(ix)) @STRANDTY@;\n\
          \        this->_nNew++;\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // finish the local-phase of a superstep by updating the strand statuses and\n\
          \  // the various counters.  Return true if there were any births/deaths\n\
          \    bool finish_step ()\n\
          \    {\n\
          \        bool anyNewDie = ((this->_nDying + this->_nNew) > 0);\n\
          \        index_t next = this->_nStable;\n\
          \        for (index_t ix = 0;  ix < this->_nStabilizing;  ix++, next++) {\n\
          \            this->_status[next] = diderot::kStable;\n\
          \        }\n\
          \        if (this->_nDying == 0) {\n\
          \          // no need to swap strands\n\
          \            index_t next = this->num_alive();\n\
          \            for (auto ix = 0;  ix < this->_nNew;  ix++, next++) {\n\
          \                this->_status[next] = diderot::kActive;\n\
          \            }\n\
          \        }\n\
          \        else {\n\
          \          // first handle the dying\n\
          \            next = this->num_alive() - this->_nDying;\n\
          \            for (index_t ix = 0;  ix < this->_nDying;  ix++, next++) {\n\
          \                this->_status[next] = diderot::kDead;\n\
          \              // invoke the dead strand's destructors\n\
          \                reinterpret_cast<@STRANDTY@ *>(this->strand(next))->~@STRANDTY@();\n\
          \            }\n\
          \          // move the new strands down over the dying strands\n\
          \            index_t src = this->num_alive();\n\
          \            index_t dst = src - this->_nDying;\n\
          \            for (auto ix = 0;  ix < this->_nNew;  ix++, dst++, src++) {\n\
          \                this->_status[dst] = diderot::kActive;\n\
          \                this->_status[src] = diderot::kDead;\n\
          \                std::swap (this->_idx[src], this->_idx[dst]);\n\
          \            }\n\
          \        }\n\
          \\n\
          \      // update counts\n\
          \        this->_nFresh = this->_nNew;\n\
          \        this->_nStable += this->_nStabilizing;\n\
          \        this->_nActive -= this->_nStabilizing + this->_nDying;\n\
          \        this->_nActive += this->_nNew;\n\
          \        this->_nStabilizing = 0;\n\
          \        this->_nNew = 0;\n\
          \        this->_nDying = 0;\n\
          \\n\
          \        return anyNewDie;\n\
          \    }\n\
          \\n\
          \  // finish a kill_all operation\n\
          \    void finish_kill_all ()\n\
          \    {\n\
          \        this->_nActive -= this->_nDying;\n\
          \        this->_nDying = 0;\n\
          \    }\n\
          \\n\
          \  // finish a stabilize_all operation\n\
          \    void finish_stabilize_all ()\n\
          \    {\n\
          \        this->_nStable += this->_nStabilizing;\n\
          \        this->_nActive -= this->_nStabilizing;\n\
          \        this->_nStabilizing = 0;\n\
          \    }\n\
          \\n\
          \  // iterator over stable strands\n\
          \    index_t begin_stable () const { return 0; }\n\
          \    index_t end_stable () const { return this->_nStable; }\n\
          \    index_t next_stable (index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // iterator over active strands\n\
          \    index_t begin_active () const { return this->_nStable+this->_nStabilizing; }\n\
          \    index_t end_active () const { return this->_nStable+this->_nActive-this->_nDying; }\n\
          \    index_t next_active (index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // iterator over alive (active+stable) strands; we assume that _nStabilizing and _nNew are 0\n\
          \    index_t begin_alive () const { return 0; }\n\
          \    index_t end_alive () const { return this->num_alive(); }\n\
          \    index_t next_alive (index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // iterator over fresh strands\n\
          \    index_t begin_fresh () const { return this->num_alive() - this->_nFresh; }\n\
          \    index_t end_fresh () const { return this->num_alive(); }\n\
          \    index_t next_fresh (index_t &ix) const { return ++ix; }\n\
          \\n\
          \  // allocate more space for strand state; return true on error\n\
          \    bool grow ()\n\
          \    {\n\
          \        size_t nItems = static_cast<size_t>(this->_nItems) + BLKSZ;\n\
          \        if (nItems >= UINT32_MAX) {\n\
          \          // cannot have more than UINT32_MAX elements\n\
          \            return true;\n\
          \        }\n\
          \\n\
          \      // allocate a new block at the end of the _blocks array\n\
          \        char *blk = static_cast<char *>(std::malloc (BLKSZ * sizeof(@STRANDTY@)));\n\
          \        if (blk == nullptr) {\n\
          \            return true;\n\
          \        }\n\
          \        this->_blocks.push_back (blk);\n\
          \\n\
          \      // grow the _status and _idx arrays\n\
          \        uint8_t *status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));\n\
          \        uint32_t *idx = static_cast<uint32_t *>(std::malloc (nItems * sizeof(uint32_t)));\n\
          \        if ((status == nullptr) || (idx == nullptr)) {\n\
          \            return true;\n\
          \        }\n\
          \        std::memcpy (status, this->_status, this->_nItems * sizeof(uint8_t));\n\
          \        std::memcpy (idx, this->_idx, this->_nItems * sizeof(uint32_t));\n\
          \\n\
          \      // initialize the new storage\n\
          \        @STRANDTY@ *p = reinterpret_cast<@STRANDTY@ *>(blk);\n\
          \        for (uint32_t ix = this->_nItems;  ix < nItems;  ix++) {\n\
          \            status[ix] = diderot::kDead;\n\
          \            idx[ix] = ix;\n\
          \        }\n\
          \\n\
          \      // free the old storage\n\
          \        std::free (this->_status);\n\
          \        std::free (this->_idx);\n\
          \\n\
          \      // update pointers\n\
          \        this->_status = status;\n\
          \        this->_idx = idx;\n\
          \        this->_nItems = nItems;\n\
          \\n\
          \        return false;\n\
          \    }\n\
          \\n\
          \}; // struct strand_array\n\
          \\n\
          \strand_array::~strand_array ()\n\
          \{\n\
          \  // run destructors to reclaim any dynamic memory attached to the strand state\n\
          \    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {\n\
          \        this->strand(ix)->~@STRANDTY@();\n\
          \    }\n\
          \    this->dealloc();\n\
          \}\n\
          \\n\
          \void strand_array::dealloc ()\n\
          \{\n\
          \    if (this->_status != nullptr) {\n\
          \        std::free (this->_status);\n\
          \        this->_status = nullptr;\n\
          \    }\n\
          \    if (this->_idx != nullptr) {\n\
          \        std::free (this->_idx);\n\
          \        this->_idx = nullptr;\n\
          \    }\n\
          \    for (uint32_t i = 0;  i < this->_blocks.size();  i++) {\n\
          \        if (this->_blocks[i] != nullptr) {\n\
          \            std::free (this->_blocks[i]);\n\
          \            this->_blocks[i] = nullptr;\n\
          \        }\n\
          \        else {\n\
          \            break;\n\
          \        }\n\
          \    }\n\
          \}\n\
          \/*---------- end seq-sarr-indirect.in ----------*/\n\
          \"

    val seqSArrayDir = "\
          \/*---------- begin seq-sarr.in ----------*/\n\
          \// forward declarations of strand methods\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_start (@START_PARAMS@@STRANDTY@ *self);\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \static diderot::strand_status @STRAND@_update (@UPDATE_PARAMS@@STRANDTY@ *self);\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \static void @STRAND@_stabilize (@STABILIZE_PARAMS@@STRANDTY@ *self);\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \\n\
          \// if we have both communication and \"die\", then we need to track when strands die\n\
          \// so that we can rebuild the list of strands use to construct the kd-tree\n\
          \#if defined(DIDEROT_HAS_STRAND_COMMUNICATION) && !defined(DIDEROT_HAS_STRAND_DIE)\n\
          \#  define TRACK_STRAND_DEATH\n\
          \#endif\n\
          \\n\
          \// strand_array for SEQUENTIAL/NO BSP/SINGLE STATE/DIRECT ACCESS\n\
          \//\n\
          \struct strand_array {\n\
          \    typedef @STRANDTY@ strand_t;\n\
          \    typedef uint32_t index_t;\n\
          \    typedef index_t sid_t;              // strand ID (index into strand-state storage)\n\
          \\n\
          \    uint8_t             *_status;       // the array of status information for the strands\n\
          \    char                *_storage;      // points to array of @STRANDTY@ structs\n\
          \    uint32_t            _nItems;        // number of items in the _storage and _status arrays\n\
          \    uint32_t            _nStable;       // number of stable strands\n\
          \    uint32_t            _nActive;       // number of active strands\n\
          \    uint32_t            _nFresh;        // number of fresh strands (new strands from create_strands)\n\
          \#ifdef TRACK_STRAND_DEATH\n\
          \    bool                _died;          // a strand died in the current superstep.\n\
          \#endif\n\
          \\n\
          \    strand_array () : _status(nullptr), _storage(nullptr), _nItems(0) { }\n\
          \    ~strand_array ();\n\
          \\n\
          \    uint32_t in_state_index () const { return 0; /* dummy */ }\n\
          \\n\
          \    uint32_t num_active () const { return this->_nActive; }\n\
          \    uint32_t num_stable () const { return this->_nStable; }\n\
          \    uint32_t num_alive () const { return this->_nActive+this->_nStable; }\n\
          \\n\
          \  // return the ID of a strand, which is the same as the ix index\n\
          \    sid_t id (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return ix;\n\
          \    }\n\
          \  // return a pointer to the strand with the given ID\n\
          \    @STRANDTY@ *id_to_strand (sid_t id) const\n\
          \    {\n\
          \        assert (id < this->_nItems);\n\
          \        return reinterpret_cast<@STRANDTY@ *>(this->_storage + id * sizeof(@STRANDTY@));\n\
          \    }\n\
          \\n\
          \  // return a strand's status\n\
          \    diderot::strand_status status (index_t ix) const\n\
          \    {\n\
          \        assert (ix < this->_nItems);\n\
          \        return static_cast<diderot::strand_status>(this->_status[ix]);\n\
          \    }\n\
          \  // return a pointer to the given strand\n\
          \    @STRANDTY@ *strand (index_t ix) const\n\
          \    {\n\
          \        return this->id_to_strand(this->id(ix));\n\
          \    }\n\
          \  // return a pointer to the local state of strand ix\n\
          \    @STRANDTY@ *local_state (index_t ix) const\n\
          \    {\n\
          \        return this->strand(ix);\n\
          \    }\n\
          \  // return a pointer to the local state of strand with the given ID\n\
          \    @STRANDTY@ *id_to_local_state (sid_t id) const\n\
          \    {\n\
          \        return this->id_to_strand(id);\n\
          \    }\n\
          \\n\
          \  // is an index valid for the strand array?\n\
          \    bool validIndex (index_t ix) const { return (ix < this->_nItems); }\n\
          \\n\
          \  // is a given strand alive?\n\
          \    bool isAlive (index_t ix) const\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        return aliveSts(this->status(ix));\n\
          \#else\n\
          \        return true;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \  // allocate space for nItems\n\
          \    bool alloc (uint32_t nItems)\n\
          \    {\n\
          \        this->_storage = static_cast<char *>(std::malloc (nItems * sizeof(@STRANDTY@)));\n\
          \        if (this->_storage == nullptr) {\n\
          \            return true;\n\
          \        }\n\
          \        this->_status = static_cast<uint8_t *>(std::malloc (nItems * sizeof(uint8_t)));\n\
          \        if (this->_status == nullptr) {\n\
          \            std::free (this->_storage);\n\
          \            return true;\n\
          \        }\n\
          \        this->_nItems = nItems;\n\
          \        this->_nActive = 0;\n\
          \        this->_nStable = 0;\n\
          \        this->_nFresh = 0;\n\
          \        return false;\n\
          \    }\n\
          \\n\
          \  // initialize the first nStrands locations as new active strands\n\
          \    void create_strands (uint32_t nStrands)\n\
          \    {\n\
          \        assert (this->_nActive == 0);\n\
          \        assert (this->_nItems == nStrands);\n\
          \        for (index_t ix = 0;  ix < nStrands;  ix++) {\n\
          \            this->_status[ix] = diderot::kActive;\n\
          \            new (this->strand(ix)) @STRANDTY@;\n\
          \        }\n\
          \        this->_nActive = nStrands;\n\
          \        this->_nFresh = nStrands;\n\
          \#ifdef TRACK_STRAND_DEATH\n\
          \        this->_died = false;\n\
          \#endif\n\
          \    }\n\
          \\n\
          \  // swap in and out states (NOP for this version)\n\
          \    void swap () { }\n\
          \\n\
          \#ifdef DIDEROT_HAS_START_METHOD\n\
          \  // invoke strand's start method\n\
          \    diderot::strand_status strand_start (@START_PARAMS@index_t ix)\n\
          \    {\n\
          \        return @STRAND@_start(@START_ARGS@this->strand(ix));\n\
          \    }\n\
          \#endif // DIDEROT_HAS_START_METHOD\n\
          \\n\
          \  // invoke strand's update method\n\
          \    diderot::strand_status strand_update (@UPDATE_PARAMS@index_t ix)\n\
          \    {\n\
          \        return @STRAND@_update(@UPDATE_ARGS@this->strand(ix));\n\
          \    }\n\
          \\n\
          \  // invoke strand's stabilize method\n\
          \    index_t strand_stabilize (@STABILIZE_PARAMS@index_t ix)\n\
          \    {\n\
          \#ifdef DIDEROT_HAS_STABILIZE_METHOD\n\
          \        @STRAND@_stabilize (@STABILIZE_ARGS@this->strand(ix));\n\
          \#endif // DIDEROT_HAS_STABILIZE_METHOD\n\
          \        this->_status[ix] = diderot::kStable;\n\
          \        this->_nActive--;\n\
          \        this->_nStable++;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // mark the given strand as dead\n\
          \    index_t kill (index_t ix)\n\
          \    {\n\
          \#ifdef TRACK_STRAND_DEATH\n\
          \        this->_died = true;\n\
          \#endif\n\
          \        this->_status[ix] = diderot::kDead;\n\
          \        this->_nActive--;\n\
          \      // skip to next active strand\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // finish the local-phase of a superstep (NOP)\n\
          \#ifdef TRACK_STRAND_DEATH\n\
          \    bool finish_step ()\n\
          \    {\n\
          \        bool res = this->_died;\n\
          \        this->_died = false;\n\
          \        return res;\n\
          \    }\n\
          \#else\n\
          \    bool finish_step () { return false; }\n\
          \#endif\n\
          \\n\
          \  // finish a kill_all operation (NOP)\n\
          \    void finish_kill_all () { }\n\
          \\n\
          \  // finish a stabilize_all operation (NOP)\n\
          \    void finish_stabilize_all () { }\n\
          \\n\
          \    index_t begin_alive () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_alive () const { return this->_nItems; }\n\
          \    index_t next_alive (index_t &ix) const\n\
          \    {\n\
          \        ix++;\n\
          \#ifdef DIDEROT_HAS_STRAND_DIE\n\
          \        while ((ix < this->_nItems) && notAliveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \#endif\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over active strands\n\
          \    index_t begin_active () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nItems) && notActiveSts(this->status(ix))) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_active () const { return this->_nItems; }\n\
          \    index_t next_active (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && notActiveSts(this->status(ix)));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over stable strands\n\
          \    index_t begin_stable () const\n\
          \    {\n\
          \        index_t ix = 0;\n\
          \        while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable)) {\n\
          \            ix++;\n\
          \        }\n\
          \        return ix;\n\
          \    }\n\
          \    index_t end_stable () const { return this->_nItems; }\n\
          \    index_t next_stable (index_t &ix) const\n\
          \    {\n\
          \        do {\n\
          \            ix++;\n\
          \        } while ((ix < this->_nItems) && (this->status(ix) != diderot::kStable));\n\
          \        return ix;\n\
          \    }\n\
          \\n\
          \  // iterator over fresh strands; since the only new strands were created by create_strand\n\
          \  // we iterate over all of them\n\
          \    index_t begin_fresh () const { return 0; }\n\
          \    index_t end_fresh () const { return this->_nFresh; }\n\
          \    index_t next_fresh (index_t &ix) const { return ++ix; }\n\
          \\n\
          \}; // struct strand_array\n\
          \\n\
          \strand_array::~strand_array ()\n\
          \{\n\
          \  // run destructors to reclaim any dynamic memory attached to the strand state\n\
          \    for (auto ix = this->begin_alive();  ix != this->end_alive();  ix = this->next_alive(ix)) {\n\
          \        this->strand(ix)->~@STRANDTY@();\n\
          \    }\n\
          \    if (this->_status != nullptr) std::free (this->_status);\n\
          \    if (this->_storage != nullptr) std::free (this->_storage);\n\
          \}\n\
          \/*---------- end seq-sarr.in ----------*/\n\
          \"

    val worldMethods = "\
          \/*---------- begin world-methods.in ----------*/\n\
          \// Allocate the program's world\n\
          \//\n\
          \world::world ()\n\
          \    : diderot::world_base (ProgramName, @IS_GRID@, @NUM_AXES@)\n\
          \{\n\
          \#ifndef DIDEROT_NO_GLOBALS\n\
          \    this->_globals = new globals;\n\
          \#endif\n\
          \\n\
          \#ifdef DIDEROT_HAS_STRAND_COMMUNICATION\n\
          \    this->_tree = nullptr;\n\
          \#endif\n\
          \} // world constructor\n\
          \\n\
          \// shutdown and deallocate the world\n\
          \//\n\
          \world::~world ()\n\
          \{\n\
          \#ifndef DIDEROT_NO_GLOBALS\n\
          \    delete this->_globals;\n\
          \#endif\n\
          \\n\
          \#ifdef DIDEROT_HAS_STRAND_COMMUNICATION\n\
          \    delete this->_tree;\n\
          \#endif\n\
          \\n\
          \} // world destructor\n\
          \\n\
          \// Initialize the program's world\n\
          \//\n\
          \bool world::init ()\n\
          \{\n\
          \    assert (this->_stage == diderot::POST_NEW);\n\
          \\n\
          \#if !defined(DIDEROT_STANDALONE_EXEC) && !defined(DIDEROT_NO_INPUTS)\n\
          \  // initialize the defined flags for the input globals\n\
          \    init_defined_inputs (this);\n\
          \#endif\n\
          \\n\
          \#ifdef DIDEROT_TARGET_PARALLEL\n\
          \  // get CPU info\n\
          \    if (this->_sched->get_cpu_info (this)) {\n\
          \        return true;\n\
          \    }\n\
          \#endif\n\
          \\n\
          \    this->_stage = diderot::POST_INIT;\n\
          \\n\
          \    return false;\n\
          \\n\
          \}\n\
          \\n\
          \// allocate the initial strands and initialize the rest of the world structure.\n\
          \//\n\
          \bool world::alloc (int32_t base[@NUM_AXES@], uint32_t size[@NUM_AXES@])\n\
          \{\n\
          \    size_t numStrands = 1;\n\
          \    for (uint32_t i = 0;  i < @NUM_AXES@;  i++) {\n\
          \        numStrands *= size[i];\n\
          \        this->_base[i] = base[i];\n\
          \        this->_size[i] = size[i];\n\
          \    }\n\
          \\n\
          \    if (this->_verbose) {\n\
          \        std::cerr << \"world::alloc: \" << size[0];\n\
          \        for (uint32_t i = 1;  i < @NUM_AXES@;  i++) {\n\
          \            std::cerr << \" x \" << size[i];\n\
          \        }\n\
          \        std::cerr << std::endl;\n\
          \    }\n\
          \\n\
          \#ifdef DIDEROT_TARGET_PARALLEL\n\
          \  // determine the block size based on the initial number of strands and the\n\
          \  // number of workers\n\
          \    this->_strands.set_block_size (this->_sched->_numWorkers, numStrands);\n\
          \#endif\n\
          \\n\
          \  // allocate the strand array\n\
          \    if (this->_strands.alloc (numStrands)) {\n\
          \        biffMsgAdd (this->_errors, \"unable to allocate strand-state array\\n\");\n\
          \        return true;\n\
          \    }\n\
          \\n\
          \  // initialize strand state pointers etc.\n\
          \    this->_strands.create_strands (numStrands);\n\
          \\n\
          \#ifdef DIDEROT_HAS_STRAND_COMMUNICATION\n\
          \    this->_tree = new diderot::kdtree<@SPATIAL_DIM@, @REALTY@, strand_array> (&this->_strands);\n\
          \#endif\n\
          \\n\
          \    return false;\n\
          \\n\
          \} // world::alloc\n\
          \\n\
          \// swap input and output states\n\
          \//\n\
          \inline void world::swap_state ()\n\
          \{\n\
          \    this->_strands.swap ();\n\
          \}\n\
          \\n\
          \#ifdef DIDEROT_HAS_KILL_ALL\n\
          \void world::kill_all ()\n\
          \{\n\
          \    if (this->_strands.num_active() > 0) {\n\
          \        for (auto ix = this->_strands.begin_active();\n\
          \            ix != this->_strands.end_active();\n\
          \            )\n\
          \        {\n\
          \            assert (this->_strands.status(ix) == diderot::kActive);\n\
          \            ix = this->_strands.kill (ix);\n\
          \        }\n\
          \        this->_strands.finish_kill_all();\n\
          \    }\n\
          \    assert (this->_strands.num_active() == 0);\n\
          \}\n\
          \#endif\n\
          \\n\
          \#ifdef DIDEROT_HAS_STABILIZE_ALL\n\
          \void world::stabilize_all ()\n\
          \{\n\
          \#ifndef DIDEROT_NO_GLOBALS\n\
          \    globals *glob = this->_globals;\n\
          \#endif\n\
          \\n\
          \    if (this->_strands.num_active() > 0) {\n\
          \        for (auto ix = this->_strands.begin_active();\n\
          \            ix != this->_strands.end_active();\n\
          \            )\n\
          \        {\n\
          \            assert (this->_strands.status(ix) == diderot::kActive);\n\
          \            this->_strands._status[ix] = diderot::kStable;\n\
          \            ix = this->_strands.strand_stabilize (@STABILIZE_ARGS_IN_WRLD@ix);\n\
          \        }\n\
          \        this->_strands.finish_stabilize_all();\n\
          \    }\n\
          \    assert (this->_strands.num_active() == 0);\n\
          \}\n\
          \#endif\n\
          \/*---------- end world-methods.in ----------*/\n\
          \"

  end
