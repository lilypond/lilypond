/*
  PROJECT: FlowerSoft C++ library
  FILE   : stringconversion.hh

*/

#ifndef __STRING_CONVERSION_HH
#define __STRING_CONVERSION_HH

/**
  all conversions from/to String go in here.( some time, anyway )
  Quite empty
  */
class StringConversion {
	static int hex2bin_i( String hex_str, String& bin_str_r );
	static int hex2nibble_i( Byte by );
	static Byte nibble2hex_by( Byte by );
public:
	static String bin2dec_str( String dec_str );
static String bin2hex_str( String bin_str );
	static String dec2bin_str( String str );
	static int dec2int_i( String str );
	static int hex2int_i( String str );
	static String hex2bin_str( String str );
	static String int2hex_str( int i, int length_i, char ch );
	static String int2dec_str( int i, int length_i, char ch );
};

#endif // __STRING_CONVERSION_HH //
