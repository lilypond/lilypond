//
// source-file.cc
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


#include "warn.hh"
#include "windhoos-suck-suck-suck-thank-you-cygnus.hh"

#include "source-file.hh"

Source_file::Source_file( String filename_str )
{
    data_caddr_ = 0;
    fildes_i_ = 0;
    size_off_ = 0;
    name_str_ = filename_str;
    istream_p_ = 0;

    open();
    map();
}

istream*
Source_file::istream_l()
{
    assert( fildes_i_ );
    if ( !istream_p_ ) {
	if ( size_off_ ) // can-t this be done without such a hack?
	    istream_p_ = new istrstream( ch_C(), size_off_ );
        else {
	    istream_p_ = new istrstream( "", 0 );
	    istream_p_->set(ios::eofbit);
	}
    }
    return istream_p_;
}

Source_file::~Source_file()
{
    delete istream_p_;
    istream_p_ = 0;
    unmap();
    close();
}

char const*
Source_file::ch_C()
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
Source_file::error_str( char const* pos_ch_C )
{
    char const* data_ch_C = ch_C();
    char const * eof_C_ = data_ch_C + size_off_;
    if ( !in_b( pos_ch_C ) )
	return "(position unknown)";

    
    if ( pos_ch_C == eof_C_)
	pos_ch_C --;
    char const* begin_ch_C = pos_ch_C;
    while ( begin_ch_C > data_ch_C )
        if ( *--begin_ch_C == '\n' ) {
	    begin_ch_C++;
	    break;
	}

    char const* end_ch_C = pos_ch_C;
    while ( end_ch_C < eof_C_ )
        if ( *end_ch_C++ == '\n' ) {
	  end_ch_C--;
	  break;
	}
  
	//    String( char const* p, int length ) is missing!?
    String line_str( (Byte const*)begin_ch_C, end_ch_C - begin_ch_C );

    int error_col_i = 0;
    char const* scan_ch_C = begin_ch_C;
    while ( scan_ch_C < pos_ch_C )
    	if ( *scan_ch_C++ == '\t' )
	    error_col_i = ( error_col_i / 8 + 1 ) * 8;
	else
	    error_col_i++;

    String str = line_str.left_str( pos_ch_C - begin_ch_C ) 
    	+ String( '\n' )
    	+ String( ' ', error_col_i ) 
	+ line_str.mid_str( pos_ch_C - begin_ch_C, INT_MAX ); // String::mid should take 0 arg..
    return str;
}

bool
Source_file::in_b( char const* pos_ch_C )
{
    return ( pos_ch_C && ( pos_ch_C >= ch_C() ) && ( pos_ch_C <= ch_C() + size_off_ ) );
}

off_t
Source_file::length_off()
{
    return size_off_;
}

int
Source_file::line_i( char const* pos_ch_C )
{
    if ( !in_b( pos_ch_C ) )
    	return 0;

    int i = 1;
    char const* scan_ch_C = ch_C();
    while ( scan_ch_C < pos_ch_C )
    	if ( *scan_ch_C++ == '\n' )
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
	warning( String( "can't map: " ) + name_str_ + String( ": " ) + strerror( errno ));
}

String
Source_file::name_str()
{
    return name_str_;
}

void
Source_file::open()
{
    fildes_i_ = ::open( name_str_, O_RDONLY );	
	    
    if ( fildes_i_ == -1 ) {
	warning( String( "can't open: " ) + name_str_ + String( ": " ) + strerror( errno )); 
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
Source_file::file_line_no_str(char const *ch_C )
{
    return name_str() + ": "
	+ String( line_i( ch_C ) );
}
