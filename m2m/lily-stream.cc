//
// lily-stream.cc
//
// source file of the LilyPond music typesetter
//
// (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

// should i be named Mudela_stream?

#include "m2m.hh"

Lily_stream::Lily_stream( String filename_str )
{
	filename_str_ = filename_str;
	os_p_ = 0;
	indent_i_ = 0;
	comment_mode_bo_ = false;
	column_i_ = 0;
	wrap_column_i_ = 78;
	open();
	header();
}

Lily_stream::~Lily_stream()
{
	delete os_p_;
	if ( indent_i_ )
		warning( "lily indent level: " + String( indent_i_ ), 0 );
}

Lily_stream&
Lily_stream::operator <<( String str )
{
	while ( str.length_i() ) {
		int max_i = wrap_column_i_ - column_i_;
		String line = str.left_str( max_i );

		while ( max_i && ( max_i < line.length_i() )
			&& ( isalnum( line[ max_i ] )
				|| ( line[ max_i ] == '\\' ) ) )
			max_i--;
		if ( max_i )
			line = str.left_str( max_i + 1 ); 
		else // cannot break neatly...
			max_i = wrap_column_i_ - column_i_ - 1;
			
		str = str.mid_str( max_i + 1, INT_MAX );
		*os_p_ << line;
		check_comment( line );
		column_i_ += line.length_i();
		if ( column_i_ >= wrap_column_i_ ) {
			//brr.
			if ( comment_mode_bo_ )
				str = "%" + str;
			newline();
		}
	}	
	return *this;
}

Lily_stream&
Lily_stream::operator <<( Midi_event& midi_event_r )
{
	midi_event_r.output_mudela( *this, false );
	return *this;
}

void
Lily_stream::check_comment( String str )
{
	int newline_i = str.index_last_i( '\n' );
	if ( newline_i != -1 ) {
		str = str.mid_str( newline_i +1, INT_MAX );
		comment_mode_bo_ = false;
	}
	if ( str.index_i( '%' ) != -1 )
		comment_mode_bo_ = true;
}

void
Lily_stream::header()
{
	*os_p_ << "% Creator: " << version_str() << "\n";
	*os_p_ << "% Automatically generated, at ";
	time_t t( time( 0 ) );
	*os_p_ << ctime( &t );
	*os_p_ << "% from input file: ";
	*os_p_ << midi_parser_l_g->filename_str_;
	*os_p_ << "\n\n";    
}

void
Lily_stream::indent()
{
	indent_i_++;
	newline();
}

void
Lily_stream::newline()
{
	*os_p_ << "\n" << String( '\t', indent_i_ );
	column_i_ = indent_i_ * 8;
	comment_mode_bo_ = false;
}

void
Lily_stream::open()
{
	os_p_ = new ofstream( filename_str_ );
	if ( !*os_p_ )
		error ( "can't open `" + filename_str_ + "\'", 0 );
}

void
Lily_stream::tnedni()
{
	assert( indent_i_ > 0 );
	indent_i_--;
	newline();
}

