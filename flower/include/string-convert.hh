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

/** The functor std::string_convert handles all conversions to/from std::string
    (some time, anyway).  The class is quite empty from data view.  */
class String_convert
{
  static int hex2bin (std::string hex_string, std::string &bin_string_r);
  static int hex2nibble (Byte byte);
  static Byte nibble2hex_byte (Byte byte);
public:
  static std::string pad_to (std::string s, int length);
  static std::string bool_string (bool b);
  static std::string bin2dec (std::string bin_string);
  static std::string bin2hex (std::string bin_string);
  static std::string dec2bin (std::string str);
  static int bin2int (std::string bin_string);
  static unsigned bin2unsigned (std::string bin_string);
  static std::string char_string (char c, int n);
  static int dec2int (std::string dec_string);
  static double dec2double (std::string dec_string);
  static std::string double_string (double f, char const *fmt = 0);
  static std::string form_string (char const *format, ...);
  static std::string vform_string (char const *format, va_list args);
  static int hex2int (std::string str);
  static unsigned hex2unsigned (std::string str);
  static std::string hex2bin (std::string str);
  static std::string int_string (int i, char const *fmt = 0);
  static std::string unsigned_string (unsigned);
  static std::string long_string (long);
  static std::string int2hex (int i, int length_i, char ch);
  static std::string unsigned2hex (unsigned u, ssize length, char ch);
  static std::string int2dec (int i, int length_i, char ch);
  static std::string rational_string (Rational);
  static std::string pointer_string (void const *);
  static std::string precision_string (double x, int n);
  static std::vector<std::string> split (std::string str, char c);
  static std::string i64_string (I64, char const *fmt = 0);
  static std::string to_lower (std::string s);
  static std::string to_upper (std::string s);
  static std::string reverse (std::string s);
};

#endif // __STRING_CONVERT_HH //
