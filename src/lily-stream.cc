//
// lily-stream.cc
//
// source file of the LilyPond music typesetter
//
// (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

// should i be named Mudela_stream?

#include <fstream.h>
#include <time.h>

#include "proto.hh"
#include "plist.hh"
#include "string.hh"

#include "moment.hh"
#include "sourcefile.hh"
#include "source.hh"
#include "midi-main.hh"    // *tors

#include "duration.hh"
#include "midi-event.hh"
#include "lily-stream.hh"

Lily_stream::Lily_stream( String filename_str )
{
	filename_str_ = filename_str;
	os_p_ = 0;
	open();
	header();
}

Lily_stream::~Lily_stream()
{
	delete os_p_;
}

Lily_stream&
Lily_stream::operator <<( String str )
{
	*os_p_ << str;
	return *this;
}

Lily_stream&
Lily_stream::operator <<( Midi_event& midi_event_r )
{
	midi_event_r.output_mudela( *this );
	return *this;
}

void
Lily_stream::header()
{
	*os_p_ << "% Creator: " << version_str() << "\n";
	*os_p_ << "% Automatically generated, at ";
	time_t t( time( 0 ) );
	*os_p_ << ctime( &t );
	*os_p_ << "% from input file: ";
//	*os_p_ << midi_parser_l_g->
	*os_p_ << "\n\n";    
}

void
Lily_stream::open()
{
	os_p_ = new ofstream( filename_str_ );
	if ( !*os_p_ )
		error ( "can't open `" + filename_str_ + "\'", 0 );
}

