#include "main.hh"

#include "string.hh"

char const * lily_version_sz();
char const * lily_version_number_sz();

String 
get_version_str()
{
  String s = lily_version_sz();
  s+="/";
  s+=flower_version_sz();
   return s;
}

String
get_version_number_str()
{
  return lily_version_number_sz();
}
