#include <fstream.h>
#include <signal.h>
#include <std/new.h>
#include <stdlib.h>
#include "debug.hh"
#include "dstream.hh"
#include "vector.hh"

Dstream monitor(&cout,".dstreamrc");
ostream * nulldev = new ofstream("/dev/null");


/*
  want to do a stacktrace .
  */
void
mynewhandler()
{
    cerr << "Out of free store memory. Aborting.. "<< flush;
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
    set_new_handler(&mynewhandler);
    set_matrix_debug(monitor);
    signal(SIGFPE, float_handler);
}   

bool check_debug=false;

void
set_debug(bool b)
{
    check_debug =b;
}


#if 0 // want to debug mem functions


/// 
static
void foobulize(void *p , size_t s)
{
//    assert(s < 2000000);
    memset(p, 0xf0, s);
}
/**
  trash a portion of memory. Make sure access to deleted stuff is bogus.
  */
void *
operator new (size_t s)
{
    void *p = malloc(s);
    assert(p);
//    foobulize(p,s);
    return p;
}

void
operator delete(void *p, size_t s)
{
    foobulize(p,s);
    free(p);
}
#endif
