/*
  PROJECT: FlowerSoft C++ library
  FILE   : stringconversion.cc

--*/


#include <assert.h>
#include "string.hh"

String
StringConversion::bin2hex_str( String bin_str )
{
    String str;
    Byte const* byte_c_l = bin_str.byte_c_l();
    for ( int i = 0; i < bin_str.length_i(); i++ ) {
	str += (char)nibble2hex_by( *byte_c_l >> 4 );
	str += (char)nibble2hex_by( *byte_c_l++ );
    }
    return str;
}

int
StringConversion::hex2bin_i( String hex_str, String& bin_str_r )
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
StringConversion::hex2bin_str( String hex_str )
{
    String str;
//  silly, asserts should alway be "on"!
//    assert( !hex2bin_i( hex_str, str ) );
    int error_i = hex2bin_i( hex_str, str );
    assert( !error_i );
    return str;
}

int 
StringConversion::hex2nibble_i( Byte by )
{
    if ( by >= '0' && by <= '9' )
        return by - '0';
    if ( by >= 'A' && by <= 'F' )
        return by - 'A' + 10;
    if ( by >= 'a' && by <= 'f')
        return by - 'a' + 10;
    return -1;
}
    
String 
StringConversion::int2hex_str( int i, int length_i, char ch )
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
StringConversion::nibble2hex_by( Byte by )
{
    if ( ( by & 0x0f ) <= 9 )
	return ( by & 0x0f ) + '0';
    else
	return ( by & 0x0f ) - 10 + 'a';
}
