#ifndef TEX_HH
#define TEX_HH

#include "string.hh"
#include "boxes.hh"

/// parameter substitution in TeXstrings
String
substitute_args(String source, svec<String> args);
/**
  this structure provides a simple macro mechanism:

  if source == "tex%bla%", then
  eval({"X","Y"})  == "texXblaY"
  */

/// #h# is in points
String vstrut(Real h);


#endif
