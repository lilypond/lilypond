#include <stdio.h>

#include "config.hh"

// #define VERSION_SZ  MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL MY_PATCH_LEVEL
#define VERSION_SZ TOPLEVEL_VERSION
static char *s = "GNU LilyPond " VERSION_SZ " #%d";


char const *
lily_version_number_sz()
{
  return VERSION_SZ;
}

char const * 
lily_version_sz()
{
	return s;
}
