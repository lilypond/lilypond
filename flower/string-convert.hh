/*
  PROJECT: FlowerSoft C++ library
  FILE   : string-convert.hh

*/

#ifndef STRING_CONVERT_HH
#define STRING_CONVERT_HH

/*
  ///a class which only has functions.
//#define functor class // cute. docxx fucks up
*/

/**
	The functor String_convert handles all conversions to/from String (some 
	time, anyway).
	The class is quite empty from data view.
  */
class String_convert {
    static int hex2bin_i( String hex_str, String& bin_str_r );
    static int hex2nibble_i( Byte byte );
    static Byte nibble2hex_byte( Byte byte );
public:
    static String bin2dec_str( String bin_str );
    static String bin2hex_str( String bin_str );
    static String dec2bin_str( String str );
    static int bin2_i( String str );
    static String char_str(char c, int n);
    static int dec2_i( String dec_str );
    static double dec2_f( String dec_str );
    static String double_str(double f, char const* fmt=0);
    static int hex2int_i( String str );
    static String hex2bin_str( String str );
    static String int_str(int i, char const *fmt=0  );
    static String i2hex_str( int i, int length_i, char ch );
    static String i2dec_str( int i, int length_i, char ch );
    static String rational_str(Rational);
    static String pointer_str(const void *);
    static String longlong_str(long long , char const * fmt = 0);
};

#endif // __STRING_CONVERT_HH //
