


/*#include <typeinfo>

signature S
{
  int foo ();
  int bar (int);
};
*/

#include <ctype.h>
#include "virtual-methods.hh"


const char *
demangle_classname (char const *s)
{
  while (isdigit (*s))
    s++;
  return s;
}
