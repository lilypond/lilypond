/*
  PROJECT: FlowerSoft C++ library
  FILE   : string-convert.cc

--*/


#include <assert.h>
#include "string.hh"

String
String_convert::bin2hex_str( String bin_str )
{
    String str;
    Byte const* byte_c_l = bin_str.byte_c_l();
    for ( int i = 0; i < bin_str.length_i(); i++ ) {
	str += (char)nibble2hex_byte( *byte_c_l >> 4 );
	str += (char)nibble2hex_byte( *byte_c_l++ );
    }
    return str;
}

int
String_convert::bin2_i( String bin_str )
{
    assert( bin_str.length_i() <= 4 );

    int result_i = 0;
    for ( int i = 0; i < bin_str.length_i(); i++ ) {
	result_i <<= 8;
	result_i += (Byte)bin_str[ i ];
    }
    return result_i;
}

// breendet imp from String
int
String_convert::dec2_i( String dec_str )
{
    if ( !dec_str.length_i() )
    	return 0;

    long l = 0;
    int conv = sscanf( dec_str.ch_c_l(), "%ld", &l );
    assert( conv );

    return (int)l;
}

// breendet imp from String
double
String_convert::dec2_f( String dec_str )
{
    if ( !dec_str.length_i() )
    	return 0;
    double d = 0;
    int conv = sscanf( dec_str.ch_c_l(), "%lf", &d );
    assert( conv );
    return d;
}

int
String_convert::hex2bin_i( String hex_str, String& bin_str_r )
{
    if ( hex_str.length_i() % 2 )
        hex_str = "0" + hex_str;

    bin_str_r = "";
    Byte const* byte_c_l= hex_str.byte_c_l();
    int i = 0;
    while ( i < hex_str.length_i() ) {   
        int high_i = hex2nibble_i( *byte_c_l++ );
        int low_i = hex2nibble_i( *byte_c_l++ );
        if ( high_i < 0 || low_i < 0 )
            return 1; // illegal char
        bin_str_r += String( (char)( high_i << 4 | low_i ), 1 );
        i += 2;
    }
    return 0;
}

String 
String_convert::hex2bin_str( String hex_str )
{
    String str;
//  silly, asserts should alway be "on"!
//    assert( !hex2bin_i( hex_str, str ) );
    int error_i = hex2bin_i( hex_str, str );
    assert( !error_i );
    return str;
}

int 
String_convert::hex2nibble_i( Byte byte )
{
    if ( byte >= '0' && byte <= '9' )
        return byte - '0';
    if ( byte >= 'A' && byte <= 'F' )
        return byte - 'A' + 10;
    if ( byte >= 'a' && byte <= 'f')
        return byte - 'a' + 10;
    return -1;
}
    
String 
String_convert::i2dec_str( int i, int length_i, char ch )
{
    char fill_ch = ch;
    if ( fill_ch)
        fill_ch = '0';

    // ugh
    String dec_str( i );
    
    // ugh
    return String( fill_ch, length_i - dec_str.length_i() ) + dec_str;
}

String 
String_convert::i2hex_str( int i, int length_i, char ch )
{
    String str;
    if ( !i )
	str = "0";
    while ( i ) {
	str = ( i % 16 )["0123456789abcdef"] + str;
	i /= 16;
    }
    if ( str.length_i() < length_i )
	str = String( ch, length_i - str.length_i() ) + str;
    return str;
}

Byte
String_convert::nibble2hex_byte( Byte byte )
{
    if ( ( byte & 0x0f ) <= 9 )
	return ( byte & 0x0f ) + '0';
    else
	return ( byte & 0x0f ) - 10 + 'a';
}
