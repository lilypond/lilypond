//
// source.cc
//

#include <assert.h>

#include "string.hh"
#include "proto.hh"
#include "plist.hh"

#include "sourcefile.hh"
#include "source.hh"

Source::Source()
{
}

Source::~Source()
{
}

void
Source::add( Source_file* sourcefile_p )
{
    sourcefile_p_iplist_.bottom().add( sourcefile_p );
}

Source_file*
Source::sourcefile_l( char const* ch_c_l )
{
    PCursor<Source_file*> sourcefile_p_pcur( sourcefile_p_iplist_.top() );
    for ( ; sourcefile_p_pcur.ok(); sourcefile_p_pcur++ )
	if ( sourcefile_p_pcur->in_b( ch_c_l ) )	
	    return *sourcefile_p_pcur;
    return 0;
}
