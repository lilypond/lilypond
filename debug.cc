#include "debug.hh"
#include "dstream.hh"
#include "vector.hh"

Dstream monitor(&cout,".dstreamrc");

void
debug_init()
{
    set_matrix_debug(monitor);
}   
