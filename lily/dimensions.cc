// dimensions.cc

#include "dimensions.hh"
#include "warn.hh"
#include "string.hh"

String
print_dimen (Real r)
{
  String s = to_str (r, "%.3f");
  if (s.index_i ("NaN") != -1)
    {
      warning (_ ("NaN"));
      s = "0.0";
    }
  s += "pt";
  return s;
}

