
#include "warn.hh"
#include "dimensions.hh"

String
dimension_str (Real r)
{
  String s = to_str (r, "%.3f");
  if (s.index_i ("NaN") != -1)
    {
      warning (_ ("NaN"));
      s = "0.0";
    }
  return s;
}

