(* fragments.sml
 *
 * This code is part of the Diderot Project (http://diderot-language.cs.uchicago.edu)
 *
 * COPYRIGHT (c) 2016 The University of Chicago
 * All rights reserved.
 *
 * !!! THIS FILE WAS GENERATED; DO NOT EDIT !!!
 *)

structure Fragments =
  struct

    val libHFoot = "\
          \/*---------- begin lib-h-foot.in ----------*/\n\
          \\n\
          \#ifdef __cplusplus\n\
          \}\n\
          \#endif\n\
          \\n\
          \#endif /* !@H_DEFINE@ */\n\
          \/*---------- end lib-h-foot.in ----------*/\n\
          \"

    val libHBody = "\
          \/*---------- begin lib-h-body.in ----------*/\n\
          \\n\
          \/***** World query operations *****/\n\
          \\n\
          \//! Return the total number of strands (active+stable) in the world\n\
          \uint32_t @PREFIX@_num_strands (@PREFIX@_world_t *wrld);\n\
          \\n\
          \//! Return the total number of active strands\n\
          \uint32_t @PREFIX@_num_active_strands (@PREFIX@_world_t *wrld);\n\
          \\n\
          \//! Return the total number of stable strands\n\
          \uint32_t @PREFIX@_num_stable_strands (@PREFIX@_world_t *wrld);\n\
          \\n\
          \//! Return true if there are any recorded error conditions\n\
          \bool @PREFIX@_any_errors (@PREFIX@_world_t *wrld);\n\
          \\n\
          \//! Return the pending error message (if any).  This call clears the pending error\n\
          \//! state.\n\
          \char *@PREFIX@_get_errors (@PREFIX@_world_t *wrld);\n\
          \\n\
          \/***** Program running operations *****/\n\
          \\n\
          \//! Allocate the program's world\n\
          \//! \\return the new world or NULL if there are any errors\n\
          \@PREFIX@_world_t *@PREFIX@_new_world ();\n\
          \\n\
          \//! Initialize the execution state for the world.  This includes allocating processor\n\
          \//! and GPU resources for parallel execution.\n\
          \//! \\param wrld the world-state of the Diderot program\n\
          \//! \\return true if there are any errors\n\
          \bool @PREFIX@_init_world (@PREFIX@_world_t *wrld);\n\
          \\n\
          \//! Initiaize the globals and create the initial set of strands\n\
          \//! \\param wrld the world-state of the Diderot program\n\
          \//! \\return true if there are any errors\n\
          \bool @PREFIX@_create_strands (@PREFIX@_world_t *wrld);\n\
          \\n\
          \//! Run the Diderot program\n\
          \//! \\param wrld the world-state of the Diderot program\n\
          \//! \\param maxNSteps the limit on the number of super steps; 0 means unlimited\n\
          \//! \\return the number of steps taken.\n\
          \uint32_t @PREFIX@_run (@PREFIX@_world_t *wrld, uint32_t maxNSteps);\n\
          \\n\
          \//! shutdown and deallocate the world\n\
          \void @PREFIX@_shutdown (@PREFIX@_world_t *wrld);\n\
          \\n\
          \/***** Runtime options *****/\n\
          \\n\
          \//! Set verbose mode\n\
          \void @PREFIX@_set_verbose (@PREFIX@_world_t *wrld, bool mode);\n\
          \\n\
          \//! Get verbose mode\n\
          \bool @PREFIX@_get_verbose (@PREFIX@_world_t *wrld);\n\
          \/*---------- end lib-h-body.in ----------*/\n\
          \"

    val libHParExtras = "\
          \/*---------- begin lib-h-par-extras.in ----------*/\n\
          \//! get the number of hardware cores\n\
          \//! \\return the number of cores on the system\n\
          \uint32_t @PREFIX@_set_num_cores (@PREFIX@_world_t *wrld);\n\
          \\n\
          \//! set the number of workers.  The value should be between 0 and the number of\n\
          \//! hardware cores.  Note that this function should be called after @PREFIX@_init_world\n\
          \//! and before @PREFIX@_create_strands.\n\
          \//! \\param wrld the world-state of the Diderot program\n\
          \//! \\param nWorkers the requested number of workers; 0 means set the number of\n\
          \//!        workers to the number of cores.\n\
          \//! \\return true if there are any errors\n\
          \bool @PREFIX@_set_num_workers (@PREFIX@_world_t *wrld, uint32_t nWorkers);\n\
          \\n\
          \//! get the number of workers.\n\
          \//! \\param wrld the world-state of the Diderot program\n\
          \//! \\return the number of workers\n\
          \uint32_t @PREFIX@_get_num_workers (@PREFIX@_world_t *wrld);\n\
          \/*---------- end lib-h-par-extras.in ----------*/\n\
          \"

    val libHHead = "\
          \/*---------- begin lib-h-head.in ----------*/\n\
          \/*! \\file @H_FILE@\n\
          \ *\n\
          \ * C interface to library generated from @SRCFILE@.\n\
          \ *\n\
          \ * Command: @DIDEROTC_CMD@ @DIDEROTC_ARGV@\n\
          \ * Version: @DIDEROTC_VERSION@\n\
          \ */\n\
          \\n\
          \#ifndef @H_DEFINE@\n\
          \#define @H_DEFINE@\n\
          \\n\
          \#define @DIDEROT_FLOAT_PRECISION@\n\
          \#define @DIDEROT_INT_PRECISION@\n\
          \#define @DIDEROT_TARGET@\n\
          \\n\
          \#include \"diderot/config.h\"\n\
          \\n\
          \#if defined(HAVE_STDBOOL_H)\n\
          \#  include <stdbool.h>\n\
          \#elif !defined(__bool_true_false_are_defined)\n\
          \#  ifndef HAVE__BOOL\n\
          \#    ifdef __cplusplus\n\
          \       typedef bool _Bool;\n\
          \#    else\n\
          \#      define _Bool signed char\n\
          \#    endif\n\
          \#  endif\n\
          \#  define bool _Bool\n\
          \#  define false 0\n\
          \#  define true 1\n\
          \#  define __bool_true_false_are_defined 1\n\
          \#endif\n\
          \\n\
          \#include <stdint.h>\n\
          \#include <string.h>\n\
          \#include \"teem/nrrd.h\"\n\
          \\n\
          \#ifdef __cplusplus\n\
          \extern \"C\" {\n\
          \#endif\n\
          \\n\
          \typedef struct @PREFIX@_struct_world @PREFIX@_world_t;\n\
          \/*---------- end lib-h-head.in ----------*/\n\
          \"

    val jsonBody = JSON.OBJECT[
                    ("program", JSON.STRING "@PROG_NAME@"),
                    ("source-file", JSON.STRING "@SRCFILE@"),
                    ("build-cmd", JSON.STRING "@DIDEROTC_CMD@ @DIDEROTC_ARGV@"),
                    ("version", JSON.STRING "@DIDEROTC_VERSION@"),
                    ("float-size", JSON.STRING "@DIDEROT_REAL_SIZE@"),
                    ("int-size", JSON.STRING "@DIDEROT_INT_SIZE@"),
                    ("target", JSON.STRING "@DIDEROT_TARGET@"),
                    ("inputs", JSON.ARRAY[]),
                    ("runtime", JSON.ARRAY[
                        JSON.OBJECT[
                            ("name", JSON.STRING "new_world"),
                            ("func", JSON.OBJECT[
                                ("return-ty", JSON.OBJECT[
                                    ("kind", JSON.STRING "*"),
                                    ("arg", JSON.STRING "@PREFIX@_world_t")
                                  ]),
                                ("name", JSON.STRING "@PREFIX@_new_world"),
                                ("params", JSON.ARRAY[])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "init_world"),
                            ("func", JSON.OBJECT[
                                ("return-ty", JSON.STRING "bool"),
                                ("name", JSON.STRING "@PREFIX@_init_world"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "create_strands"),
                            ("func", JSON.OBJECT[
                                ("return-ty", JSON.STRING "bool"),
                                ("name", JSON.STRING "@PREFIX@_create_strands"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "run"),
                            ("func", JSON.OBJECT[
                                ("return-ty", JSON.STRING "uint32_t"),
                                ("name", JSON.STRING "@PREFIX@_run"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ],
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "maxNSteps"),
                                        ("param-ty", JSON.STRING "uint32_t"),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "shutdown"),
                            ("func", JSON.OBJECT[
                                ("return-ty", JSON.STRING "void"),
                                ("name", JSON.STRING "@PREFIX@_shutdown"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "num_strands"),
                            ("get", JSON.OBJECT[
                                ("return-ty", JSON.STRING "uint32_t"),
                                ("name", JSON.STRING "@PREFIX@_num_strands"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "num_active_strands"),
                            ("get", JSON.OBJECT[
                                ("return-ty", JSON.STRING "uint32_t"),
                                ("name", JSON.STRING "@PREFIX@_num_active_strands"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "num_stable_strands"),
                            ("get", JSON.OBJECT[
                                ("return-ty", JSON.STRING "uint32_t"),
                                ("name", JSON.STRING "@PREFIX@_num_stable_strands"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "any_errors"),
                            ("get", JSON.OBJECT[
                                ("return-ty", JSON.STRING "bool"),
                                ("name", JSON.STRING "@PREFIX@_any_errors"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "get_errors"),
                            ("get", JSON.OBJECT[
                                ("return-ty", JSON.OBJECT[
                                    ("kind", JSON.STRING "*"),
                                    ("arg", JSON.STRING "char")
                                  ]),
                                ("name", JSON.STRING "@PREFIX@_get_errors"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ])
                          ],
                        JSON.OBJECT[
                            ("name", JSON.STRING "verbose"),
                            ("get", JSON.OBJECT[
                                ("return-ty", JSON.STRING "bool"),
                                ("name", JSON.STRING "@PREFIX@_get_verbose"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.OBJECT[
                                            ("kind", JSON.STRING "*"),
                                            ("arg", JSON.STRING "@PREFIX@_world_t")
                                          ]),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ]
                                  ])
                              ]),
                            ("set", JSON.OBJECT[
                                ("return-ty", JSON.STRING "void"),
                                ("name", JSON.STRING "@PREFIX@_set_verbose"),
                                ("params", JSON.ARRAY[
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "wrld"),
                                        ("param-ty", JSON.STRING "@PREFIX@_world_t*"),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "world"
                                          ])
                                      ],
                                    JSON.OBJECT[
                                        ("name", JSON.STRING "mode"),
                                        ("param-ty", JSON.STRING "bool"),
                                        ("attrbs", JSON.ARRAY[
                                            JSON.STRING "out"
                                          ])
                                      ]
                                  ])
                              ])
                          ]
                      ]),
                    ("outputs", JSON.ARRAY[])
                  ]

  end
