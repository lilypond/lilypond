//
// sourcefile.cc
//

#include <sys/types.h>		// open, mmap
#include <sys/stat.h>		// open
#include <sys/mman.h>		// mmap
#include <limits.h>		// INT_MAX
#include <fcntl.h>		// open 
#include <unistd.h>		// close, stat
#include <stdio.h>		// fdopen
#include <string.h>		// strerror
#include <errno.h>		// errno
#include <assert.h>
#include <strstream.h>

#include "string.hh"
#include "proto.hh"
#include "plist.hh"
#include "lexer.hh"
#include "debug.hh"
#include "parseconstruct.hh"
#include "main.hh"     		// find_file

#include "sourcefile.hh"

Source_file::Source_file( String &filename_str )
{
    data_caddr_ = 0;
    fildes_i_ = 0;
    size_off_ = 0;
    name_str_ = filename_str;
    istream_p_ = 0;

    open();
    map();
    // ugh!?, should call name_str() ! 
    filename_str = name_str_;
}

Source_file::~Source_file()
{
    delete istream_p_;
    istream_p_ = 0;
    unmap();
    close();
}

char const*
Source_file::ch_c_l()
{
    assert( this );
    return (char const*)data_caddr_;
}

void
Source_file::close()
{
    if ( fildes_i_ ) {
	::close( fildes_i_ );
	fildes_i_ = 0;
    }
}

String
Source_file::error_str( char const* pos_ch_c_l )
{
    assert( this );
    if ( !in_b( pos_ch_c_l ) )
	return "";

    char const* begin_ch_c_l = pos_ch_c_l;
    char const* data_ch_c_l = ch_c_l();
    while ( begin_ch_c_l > data_ch_c_l )
        if ( *--begin_ch_c_l == '\n' ) {
	    begin_ch_c_l++;
	    break;
	}

    char const* end_ch_c_l = pos_ch_c_l;
    while ( end_ch_c_l < data_ch_c_l + size_off_ )
        if ( *end_ch_c_l++ == '\n' ) {
	    break;
	}
    end_ch_c_l--;

#if 1
//    String( char const* p, int length ) is missing!?
    String line_str( (Byte const*)begin_ch_c_l, end_ch_c_l - begin_ch_c_l );
#else
    int length_i = end_ch_c_l - begin_ch_c_l;
    char* ch_p = new char[ length_i + 1 ];
    strncpy( ch_p, begin_ch_c_l, length_i );
    ch_p[ length_i ] = 0;
    String line_str( ch_p );
    delete ch_p;
#endif

    int error_col_i = 0;
    char const* scan_ch_c_l = begin_ch_c_l;
    while ( scan_ch_c_l < pos_ch_c_l )
    	if ( *scan_ch_c_l++ == '\t' )
	    error_col_i = ( error_col_i / 8 + 1 ) * 8;
	else
	    error_col_i++;

    String str = line_str.left( pos_ch_c_l - begin_ch_c_l ) 
    	+ String( '\n' )
    	+ String( ' ', error_col_i ) 
	+ line_str.mid( pos_ch_c_l - begin_ch_c_l + 1, INT_MAX ); // String::mid should take 0 arg..
    return str;
}

bool
Source_file::in_b( char const* pos_ch_c_l )
{
    return ( pos_ch_c_l && ( pos_ch_c_l >= ch_c_l() ) && ( pos_ch_c_l < ch_c_l() + size_off_ ) );
}

istream*
Source_file::istream_l()
{
    assert( fildes_i_ );
    if ( !istream_p_ ) {
	if ( size_off_ ) // can-t this be done without such a hack?
	    istream_p_ = new istrstream( ch_c_l(), size_off_ );
        else {
	    istream_p_ = new istrstream( "", 0 );
	    istream_p_->set(ios::eofbit);
	}
    }
    return istream_p_;
}

off_t
Source_file::length_off()
{
    return size_off_;
}

int
Source_file::line_i( char const* pos_ch_c_l )
{
    if ( !in_b( pos_ch_c_l ) )
    	return 0;

    int i = 1;
    char const* scan_ch_c_l = ch_c_l();
    while ( scan_ch_c_l < pos_ch_c_l )
    	if ( *scan_ch_c_l++ == '\n' )
		i++;
    return i;
}

void
Source_file::map()
{
    if ( fildes_i_ == -1 )
	return;

    data_caddr_ = (caddr_t)mmap( (void*)0, size_off_, PROT_READ, MAP_SHARED, fildes_i_, 0 );

    if ( (int)data_caddr_ == -1 )
	// ugh: defined_ch_c_l...
	warning( String( "can't map: " ) + name_str_ + String( ": " ) + strerror( errno ), defined_ch_c_l ); //lexer->here_ch_c_l() );
}

String
Source_file::name_str()
{
    return name_str_;
}

void
Source_file::open()
{
    String name_str = find_file( name_str_ );
    if ( name_str != "" ) 
        name_str_ = name_str;

    fildes_i_ = ::open( name_str_, O_RDONLY );	
	    
    if ( fildes_i_ == -1 ) {
	warning( String( "can't open: " ) + name_str_ + String( ": " ) + strerror( errno ), defined_ch_c_l ); // lexer->here_ch_c_l() );
        return;
    }

    struct stat file_stat;
    fstat( fildes_i_, &file_stat );
    size_off_ = file_stat.st_size;
}

void
Source_file::unmap()
{
    if ( data_caddr_ ) {
	munmap( data_caddr_, size_off_ );
    	data_caddr_ = 0;
	size_off_ = 0;
    }
}
String
Source_file::file_line_no_str(char const *ch_c_l )
{
    return name_str() + ": "
	+ String( line_i( ch_c_l ) );
}
