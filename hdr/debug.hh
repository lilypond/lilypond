#ifndef DEBUG_HH
#define DEBUG_HH
#include <assert.h>
#include <iostream.h>
#include "dstream.hh"

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
#define mtor monitor.identify_as(__PRETTY_FUNCTION__)
#endif



#endif
