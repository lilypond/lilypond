#ifndef DEBUG_HH
#define DEBUG_HH
#include <assert.h>
#include <iostream.h>
#include "dstream.hh"



#define WARN warnout << "warning: "<<__FUNCTION__ << "(): "
extern ostream &warnout ;
extern ostream *mlog;  
extern ostream *nulldev;  
extern Dstream monitor; // monitor

#ifdef NPRINT
#define mtor *nulldev
#else
#define mtor monitor.identify_as(__PRETTY_FUNCTION__)
#endif
void error(String s);
void warning(String s);

#endif
