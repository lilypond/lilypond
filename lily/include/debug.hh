/*
  Argh! this sux! implementation not liblily
 */

#ifndef DEBUG_HH
#define DEBUG_HH

#include <assert.h>
#include <iostream.h>
#include "dstream.hh"
#include "real.hh"
#include "lily-proto.hh"
#include "warn.hh"

void error_t (const String& s, Time_description const &  t_tdes);
void error_t (String const &s, const Moment &when);

#define WARN warnout << "warning: "<<__FUNCTION__ << "(): "
extern ostream &warnout ;

// progress
extern ostream *mlog;

// debugging
extern Dstream *lily_monitor; // monitor

#ifdef NPRINT
/** 
  Global debug output. Compare with cin, cout, cerr, DOUT is DEBUG OUTPUT
 */
#define DOUT if (0) *lily_monitor	// clever hack 
#else
#define DOUT if (check_debug) lily_monitor->identify_as (__PRETTY_FUNCTION__)
#endif

extern bool check_debug;

#endif
