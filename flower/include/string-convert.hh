/*
  PROJECT: FlowerSoft C++ library
  FILE   : string-convert.hh

*/

#ifndef STRING_CONVERT_HH
#define STRING_CONVERT_HH

#include <stdarg.h>
#include "flower-proto.hh"
#include "string.hh"


/** The functor String_convert handles all conversions to/from String
 (some time, anyway).  The class is quite empty from data view.  */
class String_convert {
  static int hex2bin (String hex_string, String& bin_string_r);
  static int hex2nibble (Byte byte);
  static Byte nibble2hex_byte (Byte byte);
public:
  static String pad_to (String s, int length);
  static String bool_string (bool b);
  static String bin2dec (String bin_string);
  static String bin2hex (String bin_string);
  static String dec2bin (String str);
  static int bin2int (String bin_string);
  static unsigned bin2unsigned (String bin_string);
  static String char_string (char c, int n);
  static int dec2int (String dec_string);
  static double dec2double (String dec_string);
  static String double_string (double f, char const* fmt=0);
  static String form_string (char const* format, ...);
  static String vform_string (char const* format, va_list args);
  static int hex2int (String str);
  static unsigned hex2unsigned (String str);
  static String hex2bin (String str);
  static String int_string (int i, char const *fmt=0 );
  static String long_string (long);
  static String int2hex (int i, int length_i, char ch);
  static String unsigned2hex (unsigned u, int length_i, char ch);
  static String int2dec (int i, int length_i, char ch);
  static String rational_string (Rational);
  static String pointer_string (void const *);
  static String precision_string (double x, int n);
  static Array<String> split (String str, char c);
  static String i64_string (I64, char const * fmt = 0);
};

#endif // __STRING_CONVERT_HH //
