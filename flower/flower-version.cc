#include <stdio.h>

#include "version.hh"

static char *s = "FlowerLib " MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL "." MY_PATCH_LEVEL;
//static char *s = "FlowerLib " FLOWER_VERSION ;


char const * 
flower_version_sz()
{
  return s;
}
