#include "main.hh"
#include "string.hh"

char const * lily_version_sz();

String 
get_version_str()
{
  String s = lily_version_sz();
  return s;
}
