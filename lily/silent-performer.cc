/*
  silent-performer.cc -- implement Silent_performer

  (c) 1996, 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */

#include "silent-performer.hh"
#include "translator.hh"
#include "input-translator.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B1(Silent_performer,Performer);
IMPLEMENT_STATIC_NAME(Silent_performer);
ADD_THIS_PERFORMER(Silent_performer);

Silent_performer::Silent_performer()
{
}

Silent_performer::~Silent_performer()
{
}

bool 
Silent_performer::try_request( Request* req_l )
{
    return true;
#if 0
    Command_req* com_l = req_l->command();
    Musical_req* mus_l = req_l->musical();

    if ( com_l ) {
	if ( com_l->bar() )
	    return true;

	if ( com_l->barcheck() )
	    return true;
    }

    if ( mus_l ) {
	if ( mus_l->beam() )
	    return true;
	
	if( mus_l->rest() )
	    return true;

	if( mus_l->slur() )
	    return true;

	if ( mus_l->stem() )
	    return true;

	if ( mus_l->tie() )
	    return true;
    }

    return false;
#endif
}

