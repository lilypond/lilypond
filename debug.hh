#ifndef DEBUG_HH
#define DEBUG_HH
#include <assert.h>
#include <iostream.h>
#include "dstream.hh"



#define WARN warnout << "warning: "<<__FUNCTION__ << ": "
extern ostream &warnout ;
extern ostream *mlog;  
extern dstream mtor; // monitor

void error(String s);
void warning(String s);

extern int debug_flags;

void
set_debug(String s);
const int DEBUGPARSER = 0x01;
const int DEBUGTOKEN = 0x02;
const int DEBUGITEMS = 0x04;

#endif
