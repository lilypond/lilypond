/*
  PROJECT: FlowerSoft C++ library
  FILE   : string-convert.cc

--*/

#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include "libc-extension.hh"
#include "string.hh"
#include "string-convert.hh"
#include "rational.hh"
#include "array.hh"

/**
   A safe length for stringconversion buffers.

   worst case would be %f printing HUGE (or 1/HUGE), which is approx
   2e318, this number would have approx 318 zero's in its string.

   Should enlarge buff dynamically.
   
   @see
   man 3 snprintf
   */
static const int STRING_BUFFER_LEN=1024;

String
String_convert::bool_string (bool b)
{
  return String (b ? "true" : "false");
}

String
String_convert::bin2hex (String bin_string)
{
  String str;
  Byte const* byte = bin_string.to_bytes ();
  for (int i = 0; i < bin_string.length (); i++) 
    {
      str += to_string ((char)nibble2hex_byte (*byte >> 4));
      str += to_string ((char)nibble2hex_byte (*byte++));
    }
  return str;
}

int
String_convert::bin2int (String bin_string)
{
  return bin2unsigned (bin_string);
}

unsigned
String_convert::bin2unsigned (String bin_string)
{
  assert (bin_string.length () <= (int)sizeof (unsigned));

  unsigned result_u = 0;
  for (int i = 0; i < bin_string.length (); i++) 
    {
      result_u <<= 8;
      result_u += (Byte)bin_string[ i ];
    }
  return result_u;
}

// breendet imp from String
int
String_convert::dec2int (String dec_string)
{
  if (!dec_string.length ())
    return 0;

  long l = 0;
  int conv = sscanf (dec_string.to_str0 (), "%ld", &l);
  assert (conv);

  return (int)l;
}

String
String_convert::i64_string (I64 i64, char const* fmt)
{
  char buffer[STRING_BUFFER_LEN];
  snprintf (buffer, STRING_BUFFER_LEN,
 (fmt ? fmt : "%Ld"), i64);     // assume radix 10
  return String (buffer);

}
// breendet imp from String
double
String_convert::dec2double (String dec_string)
{
  if (!dec_string.length ())
    return 0;
  double d = 0;
  int conv = sscanf (dec_string.to_str0 (), "%lf", &d);
  assert (conv);
  return d;
}

int
String_convert::hex2bin (String hex_string, String& bin_string_r)
{
  if (hex_string.length () % 2)
    hex_string = "0" + hex_string;

  bin_string_r = "";
  Byte const* byte= hex_string.to_bytes ();
  int i = 0;
  while (i < hex_string.length ()) 
    {
      int high_i = hex2nibble (*byte++);
      int low_i = hex2nibble (*byte++);
      if (high_i < 0 || low_i < 0)
	return 1; // illegal char
      bin_string_r += to_string ((char) (high_i << 4 | low_i), 1 );
      i += 2;
    }
  return 0;
}

String 
String_convert::hex2bin (String hex_string)
{
  String str;
  //  silly, asserts should alway be "on"!
  //    assert (!hex2bin (hex_string, str) );
  int error_i = hex2bin (hex_string, str);
  assert (!error_i);
  return str;
}

int 
String_convert::hex2nibble (Byte byte)
{
  if (byte >= '0' && byte <= '9')
    return byte - '0';
  if (byte >= 'A' && byte <= 'F')
    return byte - 'A' + 10;
  if (byte >= 'a' && byte <= 'f')
    return byte - 'a' + 10;
  return -1;
}

// stupido.  Should use int_string ()
String 
String_convert::int2dec (int i, int length_i, char ch)
{
  char fill_char = ch;
  if (fill_char)
    fill_char = '0';

  // ugh
  String dec_string = to_string (i);
  
  // ugh
  return to_string (fill_char, length_i - dec_string.length ()) + dec_string;
}


// stupido.  Should use int_string ()
String 
String_convert::unsigned2hex (unsigned u, int length_i, char fill_char)
{
  String str;
  if (!u)
    str = "0";

#if 1 // both go...
  while (u) 
    {
      str = to_string ((char) ((u % 16)["0123456789abcdef"] ) ) + str;
      u /= 16;
    }
#else
  str += int_string (u, "%x");	// hmm. %lx vs. %x -> portability?
#endif

  str = to_string (fill_char, length_i - str.length ()) + str;
  while ((str.length () > length_i) && (str[ 0 ] == 'f' ) )
    str = str.cut_string (2, INT_MAX);

  return str;
}

String 
String_convert::int2hex (int i, int length_i, char fill_char)
{
  return unsigned2hex ((unsigned)i, length_i, fill_char);
}

Byte
String_convert::nibble2hex_byte (Byte byte)
{
  if ((byte & 0x0f) <= 9 )
    return (byte & 0x0f) + '0';
  else
    return (byte & 0x0f) - 10 + 'a';
}
/**
  Convert an integer to a string

  @param
  #fmt# is a printf style format, default assumes "%d" as format. 
  */
String
String_convert::int_string (int i, char const* fmt)
{
  char buffer[STRING_BUFFER_LEN];
  snprintf (buffer, STRING_BUFFER_LEN,
 (fmt ? fmt : "%d"), i);     // assume radix 10
  return String (buffer);
}

String
String_convert::form_string (char const* format, ...)
{
  va_list args;
  va_start (args, format);
  char buffer[STRING_BUFFER_LEN];
  vsnprintf (buffer, STRING_BUFFER_LEN, format, args);
  va_end (args);
  return String (buffer);
}

String 
String_convert::vform_string (char const* format, va_list args)
{
  char buffer[STRING_BUFFER_LEN];
  vsnprintf (buffer, STRING_BUFFER_LEN, format, args);
  return String (buffer);
}

/**
  Convert a double to a string.

  @param #fmt# is a printf style format, default assumes "%lf" as format
 */
String
String_convert::double_string (double f, char const* fmt)
{
  char buf[STRING_BUFFER_LEN]; 

  snprintf (buf, STRING_BUFFER_LEN, fmt ? fmt : "%f", f);
  return String (buf);
}

/**
Make a string from a single character.

  @param
  #n# is a repetition count, default value is 1
 */
String
String_convert::char_string (char c, int n)
{
  n = n >= 0 ? n : 0;
  char* ch = new char[ n ];
  memset (ch, c, n);
  String s ((Byte*)ch, n);
  delete[] ch;
  return s;
}

String
String_convert::rational_string (Rational r)
{
	return r.string ();
}

String
String_convert::pointer_string (void const *l)
{
  char buffer[STRING_BUFFER_LEN];
  snprintf (buffer, STRING_BUFFER_LEN, "%p", l);     // assume radix 10
  return String (buffer);
}

/**
  Convert a double to a string.

  @param
  #n# is the number of nonzero digits
 */
String
String_convert::precision_string (double x, int n)
{
  String format = "%." + to_string (0 >? n - 1) + "e";
  String str = double_string (abs (x), format.to_str0 ());

  int exp = str.right_string (3).to_int ();
  str = str.left_string (str.length () - 4);

  while (str[str.length () - 1] == '0')
    str = str.left_string (str.length () - 1);
  if (str[str.length () - 1] == '.')
    str = str.left_string (str.length () - 1);

  if (exp == 0)
    return (sign (x) > 0 ? str : "-" + str);

  str = str.left_string (1) + str.cut_string (2, INT_MAX);
  int dot = 1 + exp;
  if (dot <= 0)
    str = "0." + to_string ('0', -dot) + str;
  else if (dot >= str.length ())
    str += to_string ('0', dot - str.length ());
  else if (( dot > 0) && (dot < str.length ()))
    str = str.left_string (dot) + "." + str.cut_string (dot, INT_MAX);
  else
    assert (0);

  return (sign (x) > 0 ? str : "-" + str);
}

Array<String>
String_convert::split (String str, char c)
{
  Array<String> a;
  int i = str.index (c);
  while (i >=0)
    {
      String s = str.left_string (i);
      a.push (s);
      while (str[++i] == c)
	;
      str = str.cut_string (i, INT_MAX);
      i = str.index (c);
    }
  if (str.length ())
    a.push (str);
  return a;
}


String
String_convert::long_string (long l)
{
  char s[STRING_BUFFER_LEN];
  sprintf (s,"%ld", l);
  return s;
}

String
String_convert::pad_to (String s, int n)
{
  return s + to_string (' ' , (n - s.length ()) >? 0);
}
