#include <stdio.h>

#include "config.hh"

// static char *s = "FlowerLib " MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL MY_PATCH_LEVEL " #%d";
static char *s = "FlowerLib " MODULE_VERSION " #%d";

static const int build=
//#include ".build"
0
;

char const * 
flower_version_sz()
{
  static char v[1024];
  sprintf (v, s, build);
  return v;
}
