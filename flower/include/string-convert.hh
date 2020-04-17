/*
  PROJECT: FlowerSoft C++ library
  FILE   : std::string-convert.hh
*/

#ifndef STRING_CONVERT_HH
#define STRING_CONVERT_HH

#include <cstdarg>

#include "flower-proto.hh"
#include "std-string.hh"

/** The functor string_convert handles all conversions to/from std::string
    (some time, anyway).  The class is quite empty from data view.  */
class String_convert
{
  static int hex2bin (std::string hex_string, std::string &bin_string_r);
  static int hex2nibble (Byte byte);
  static Byte nibble2hex_byte (Byte byte);
public:
  static std::string be_u32 (uint32_t u);
  static std::string be_u24 (uint32_t u);
  static std::string be_u16 (uint16_t u);
  static std::string pad_to (const std::string &s, size_t length);
  static std::string bin2hex (Byte bin_char);
  static std::string bin2hex (const std::string &bin_string);
  static int dec2int (const std::string &dec_string);
  static std::string form_string (char const *format, ...) __attribute__ ((format (printf, 1, 2)));
  static std::string vform_string (char const *format, va_list args);
  static std::string hex2bin (const std::string &str);
  static std::string int_string (int i, char const *fmt = 0);
  static std::string int2hex (int i, size_t length_i, char ch);
  static std::string unsigned2hex (unsigned u, size_t length, char ch);
  static std::string to_lower (std::string s);
  static std::string to_upper (std::string s);
};

#endif // __STRING_CONVERT_HH //
