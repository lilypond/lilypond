#ifndef DEBUG_HH
#define DEBUG_HH
#include <assert.h>
#include <iostream.h>
#include "dstream.hh"
#include "real.hh"

void error(String s);		// errors

// warnings
void warning(String s);
#define WARN warnout << "warning: "<<__FUNCTION__ << "(): "
extern ostream &warnout ;

// progress
extern ostream *mlog;

// debugging
extern Dstream monitor; // monitor

#ifdef NPRINT
#define mtor if (0) monitor	// clever hack 
#else
#define mtor if (check_debug) monitor.identify_as(__PRETTY_FUNCTION__)
#endif

extern bool check_debug;

#endif
