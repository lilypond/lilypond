#include "debug.hh"
#include "dstream.hh"
int debug_flags;

void
set_debug(String s)
{
    if (s.pos ('t')) {
	debug_flags |= DEBUGTOKEN;
	mtor << " Turning on token debug\n";
    }
    if (s.pos ('p')){
	debug_flags |= DEBUGPARSER;
	mtor << "Turning on parser debugger\n";
    }
}

Dstream monitor(cout,".dstreamrc");
