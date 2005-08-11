#include <cctype>

#include "virtual-methods.hh"

char const *
demangle_classname (std::type_info const &t)
{
  char const *s = t.name ();
  while (isdigit (*s))
    s++;
  return s;
}
