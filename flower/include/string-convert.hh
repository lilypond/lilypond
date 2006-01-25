/*
  PROJECT: FlowerSoft C++ library
  FILE   : string-convert.hh
*/

#ifndef STRING_CONVERT_HH
#define STRING_CONVERT_HH

#include <cstdarg>
using namespace std;

#include "std-string.hh"
#include "string.hh"

/** The functor Std_string_convert handles all conversions to/from Std_string
    (some time, anyway).  The class is quite empty from data view.  */
class String_convert
{
  static int hex2bin (Std_string hex_string, Std_string &bin_string_r);
  static int hex2nibble (Byte byte);
  static Byte nibble2hex_byte (Byte byte);
public:
  static Std_string pad_to (Std_string s, int length);
  static Std_string bool_string (bool b);
  static Std_string bin2dec (Std_string bin_string);
  static Std_string bin2hex (Std_string bin_string);
  static Std_string dec2bin (Std_string str);
  static int bin2int (Std_string bin_string);
  static unsigned bin2unsigned (Std_string bin_string);
  static Std_string char_string (char c, int n);
  static int dec2int (Std_string dec_string);
  static double dec2double (Std_string dec_string);
  static Std_string double_string (double f, char const *fmt = 0);
  static Std_string form_string (char const *format, ...);
  static Std_string vform_string (char const *format, va_list args);
  static int hex2int (Std_string str);
  static unsigned hex2unsigned (Std_string str);
  static Std_string hex2bin (Std_string str);
  static Std_string int_string (int i, char const *fmt = 0);
  static Std_string long_string (long);
  static Std_string int2hex (int i, int length_i, char ch);
  static Std_string unsigned2hex (unsigned u, ssize length, char ch);
  static Std_string int2dec (int i, int length_i, char ch);
  static Std_string rational_string (Rational);
  static Std_string pointer_string (void const *);
  static Std_string precision_string (double x, int n);
  static Array<Std_string> split (Std_string str, char c);
  static Std_string i64_string (I64, char const *fmt = 0);
};

#endif // __STRING_CONVERT_HH //
