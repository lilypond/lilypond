//
// lily-stream.cc
//
// source file of the LilyPond music typesetter
//
// (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

// should i be named Mudela_stream?

#include "mi2mu.hh"

Lily_stream::Lily_stream( String filename_str )
{
    filename_str_ = filename_str;
    os_p_ = 0;
    indent_i_ = 0;
    comment_mode_b_ = false;
    column_i_ = 0;
    wrap_column_i_ = 60;
    open();
    header();
}

Lily_stream::~Lily_stream()
{
    delete os_p_;
    if ( indent_i_ )
	warning( "lily indent level: " + String( indent_i_ ));
}

Lily_stream&
Lily_stream::operator <<( String str )
{
    static String word_sep_str = "{} \t\n";
    while ( str.length_i() ) {
	int i = str.index_any_i( word_sep_str ) + 1;
	if ( !i )
	    i = str.length_i();
	String word = str.left_str( i );
	str = str.mid_str( i, str.length_i() );
	output_wrapped( word );
    }
    return *this;
}

Lily_stream&
Lily_stream::operator <<( Midi_event& midi_event_r )
{
    midi_event_r.output_mudela( *this, false );
    *os_p_ << flush;
    return *this;
}

void
Lily_stream::header()
{
    *os_p_ << "% Creator: " << mi2mu_version_str() << "\n";
    *os_p_ << "% Automatically generated, at ";
    time_t t( time( 0 ) );
    *os_p_ << ctime( &t );
    *os_p_ << "% from input file: ";
    *os_p_ << midi_parser_l_g->filename_str_;
    *os_p_ << "\n\n";    
    // ugh
    *os_p_ << "\\version \"0.1.0\";\n";
}

void
Lily_stream::open()
{
    os_p_ = new ofstream( filename_str_ );
    if ( !*os_p_ )
	error ( "can't open `" + filename_str_ + "\'");
}

void
Lily_stream::output( String str )
{
    for ( int i = 0; i < str.length_i(); i++ ) {
	char c = str[ i ];
	switch ( c ) {
	    case '{' :
	    case '<' :
		indent_i_++;
		column_i_++;
		*os_p_ << c;
		break;
	    case '}' :
	    case '>' :
		assert( indent_i_ );
		indent_i_--;
		column_i_++;
		*os_p_ << c;
		break;
	    case '%' :
		comment_mode_b_ = true;
		*os_p_ << c;
		column_i_++;
		break;
	    case '\t' :
		column_i_ += 8;
		*os_p_ << c;
		break;
	    case '\n' :
		*os_p_ << endl;
		*os_p_ << String( '\t', indent_i_ );
		column_i_ = indent_i_ * 8;
		comment_mode_b_ = false;
		break;
	    default :
		column_i_++;
		*os_p_ << c;
		break;
	}	
    }
}

void
Lily_stream::output_wrapped( String str )
{
    // enough room left -> doit
    if ( column_i_ + str.length_i() <= wrap_column_i_ ) {
	output( str );
	return;
    }

    // we're at BOL already; this will never fit -> doit
    if ( column_i_ == indent_i_ * 8 ) {
	output( str );
	return;
    }
    
    // ok, let's wrap
    // preserve comment mode
    if ( comment_mode_b_ )
	output( String( "\n%" ) );
    else 
	output( String( "\n" ) );
    
    output( str );
}


