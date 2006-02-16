/*
  PROJECT: FlowerSoft C++ library
  FILE   : string-convert.hh
*/

#ifndef STRING_CONVERT_HH
#define STRING_CONVERT_HH

#include <cstdarg>
using namespace std;

#include "std-string.hh"
#include "flower-proto.hh"

/** The functor string_convert handles all conversions to/from string
    (some time, anyway).  The class is quite empty from data view.  */
class String_convert
{
  static int hex2bin (string hex_string, string &bin_string_r);
  static int hex2nibble (Byte byte);
  static Byte nibble2hex_byte (Byte byte);
public:
  static string pad_to (string s, int length);
  static string bool_string (bool b);
  static string bin2dec (string bin_string);
  static string bin2hex (string bin_string);
  static string dec2bin (string str);
  static int bin2int (string bin_string);
  static unsigned bin2unsigned (string bin_string);
  static string char_string (char c, int n);
  static int dec2int (string dec_string);
  static double dec2double (string dec_string);
  static string double_string (double f, char const *fmt = 0);
  static string form_string (char const *format, ...);
  static string vform_string (char const *format, va_list args);
  static int hex2int (string str);
  static unsigned hex2unsigned (string str);
  static string hex2bin (string str);
  static string int_string (int i, char const *fmt = 0);
  static string unsigned_string (unsigned);
  static string long_string (long);
  static string int2hex (int i, int length_i, char ch);
  static string unsigned2hex (unsigned u, ssize length, char ch);
  static string int2dec (int i, int length_i, char ch);
  static string rational_string (Rational);
  static string pointer_string (void const *);
  static string precision_string (double x, int n);
  //  static vector<string> split (string str, char c);
  static string i64_string (I64, char const *fmt = 0);
  static string to_lower (string s);
  static string to_upper (string s);
  static string reverse (string s);
};

#endif // __STRING_CONVERT_HH //
