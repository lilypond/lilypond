#include "main.hh"

#include "string.hh"

extern const char* lily_version_sz();

String 
get_version_str()
{
    String s = lily_version_sz();
    s+="/";
    s+=flower_version_sz();
   return s;
}
