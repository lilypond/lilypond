#include <fstream.h>
#include <signal.h>
#include <std/new.h>
#include <stdlib.h>
#include "debug.hh"
#include "dstream.hh"
#include "flower-debug.hh"

Dstream *monitor=0;
ostream * nulldev =0;
// ugh
struct _Dinit {
    _Dinit() {
	nulldev = new ofstream("/dev/null");
	monitor = new Dstream(&cout,".dstreamrc");
    }
    ~_Dinit() {
	delete nulldev;
	delete monitor;
    }
} dinit;



/*
  want to do a stacktrace .
  */
void
mynewhandler()
{
    assert(false);
}

void
float_handler(int)
{
    cerr << "Floating point exception .. \n"<< flush;
    assert(false);
}

void
debug_init()
{
#ifndef NDEBUG
    set_new_handler(&mynewhandler);
#endif
    set_flower_debug(*monitor, check_debug);
    
    signal(SIGFPE, float_handler);
}   

bool check_debug=false;

void
set_debug(bool b)
{
    check_debug =b;
    set_flower_debug(*monitor, check_debug);
  }


