#include <fstream.h>
#include <std/new.h>
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
debug_init()
{
    set_new_handler(&mynewhandler);
    set_matrix_debug(monitor);
}   

bool check_debug=false;

void
set_debug(bool b)
{
    check_debug =b;
}
