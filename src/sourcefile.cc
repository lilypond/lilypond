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
#include "main.hh"     		// find_file

#include "sourcefile.hh"

Source_file::Source_file( String &filename_str )
{
    data_caddr_m = 0;
    fildes_i_m = 0;
    size_off_m = 0;
    name_str_m = filename_str;
    istream_p_m = 0;

    open();
    map();
    filename_str = name_str_m;
}

Source_file::~Source_file()
{
    delete istream_p_m;
    istream_p_m = 0;
    unmap();
    close();
}

char const*
Source_file::ch_c_l()
{
    assert( this );
    return (char const*)data_caddr_m;
}

void
Source_file::close()
{
    if ( fildes_i_m ) {
	::close( fildes_i_m );
	fildes_i_m = 0;
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
    while ( end_ch_c_l < data_ch_c_l + size_off_m )
        if ( *end_ch_c_l++ == '\n' ) {
	    break;
	}
    end_ch_c_l--;

// String( char const* p, int length ) is missing!?
//    String line_str( begin_ch_c_l, end_ch_c_l - begin_ch_c_l );

    int length_i = end_ch_c_l - begin_ch_c_l;
    char* ch_p = new char[ length_i + 1 ];
    strncpy( ch_p, begin_ch_c_l, length_i );
    ch_p[ length_i ] = 0;
    String line_str( ch_p );
    delete ch_p;

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
    return ( pos_ch_c_l && ( pos_ch_c_l >= ch_c_l() ) && ( pos_ch_c_l < ch_c_l() + size_off_m ) );
}

istream*
Source_file::istream_l()
{
    assert( fildes_i_m );
    if ( !istream_p_m ) {
	if ( size_off_m ) // can-t this be done without such a hack?
	    istream_p_m = new istrstream( ch_c_l(), size_off_m );
        else {
	    istream_p_m = new istrstream( "", 0 );
	    istream_p_m->set(ios::eofbit);
	}
    }
    return istream_p_m;
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
    data_caddr_m = (caddr_t)mmap( (void*)0, size_off_m, PROT_READ, MAP_SHARED, fildes_i_m, 0 );

    if ( (int)data_caddr_m == -1 ) {
	cerr << "lilypond: can't map: " << name_str_m << ": " << strerror( errno ) << endl;
	assert( 0 );
    }
}

String
Source_file::name_str()
{
    return name_str_m;
}

void
Source_file::open()
{
    name_str_m = find_file( name_str_m );
    fildes_i_m = ::open( name_str_m, O_RDONLY );	
    
    if ( fildes_i_m == -1 ) {
	cerr << "lilypond: can't open: " << name_str_m << ": " << strerror( errno ) << endl;
	assert( 0 );
    }

    struct stat file_stat;
    fstat( fildes_i_m, &file_stat );
    size_off_m = file_stat.st_size;
}

void
Source_file::unmap()
{
    if ( data_caddr_m ) {
	munmap( data_caddr_m, size_off_m );
    	data_caddr_m = 0;
	size_off_m = 0;
    }
}
String
Source_file::file_line_no_str(const char *cch_c_l )
{
    return name_str() + ": "
	+ String( line_i( cch_c_l ) );
}
