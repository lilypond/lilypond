#include <cctype>
using namespace std;

#include "virtual-methods.hh"

char const *
demangle_classname (type_info const &t)
{
  char const *s = t.name ();
  while (isdigit (*s))
    s++;
  return s;
}
