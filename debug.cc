#include <fstream.h>
#include "debug.hh"
#include "dstream.hh"
#include "vector.hh"

Dstream monitor(&cout,".dstreamrc");
ostream * nulldev = new ofstream("/dev/null");
void
debug_init()
{
    set_matrix_debug(monitor);
}   
