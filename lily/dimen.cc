#include <ctype.h>
#include "dimension.hh"
#include "debug.hh"
#include "string.hh"

Real
parse_dimen (String dim)
{
  int i=dim.length_i()-1;
  char const *s = dim.ch_C ();
  while  (i > 0 && (isspace (s[i]) || isalpha (s[i])))
    {
      i--;
    }
  String unit (s + i+1);
  return convert_dimen (dim.value_f(), unit);
}


Real
convert_dimen (Real quant, String unit)
{
  if (unit == "cm")
    return quant * CM_TO_PT;
  if (unit == "pt")
    return quant;
  if (unit == "mm")
    return quant*CM_TO_PT/10;
  if (unit == "in")
    return quant * INCH_TO_PT;
  error (_f ("unknown length unit: `%s\'", unit));
}

String
print_dimen (Real r)
{
  String s = to_str (r, "%.3f");
  if (s.index_i ("NaN") != -1)
    {
      warning (_ ("NaN"));
      s = "0.0";
    }
  s += "pt ";
  return s;
}

