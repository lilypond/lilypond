#ifndef TEX_HH
#define TEX_HH

#include "string.hh"
#include "boxes.hh"
#include "scalar.hh"

/** parameter substitution in TeXstrings.
  this function provides a simple macro mechanism:

  if source == "tex%bla%", then
  substitute_args(source, {"X","Y"})  == "texXblaY"
  */
String
substitute_args(String source, Array<String> args);

/// parameter substitution in TeXstrings
String
substitute_args(String source, Array<Scalar> args);

/// #h# is in points
String vstrut(Real h);


#endif
