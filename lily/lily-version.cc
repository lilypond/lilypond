#include "version.hh"
#include <stdio.h>


#define VERSION_SZ  MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL MY_PATCH_LEVEL
static char *s = "GNU LilyPond " VERSION_SZ " #%d";

static const int build=
#include ".build"
;

char const *
lily_version_number_sz()
{
  return VERSION_SZ;
}

char const * 
lily_version_sz()
{
  static char v[1024];	// ugh
  sprintf (v, s, build);
  return v;
}
