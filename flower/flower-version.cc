#include <stdio.h>

#include "config.hh"

// static char *s = "FlowerLib " MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL MY_PATCH_LEVEL " #%d";
static char *s = "FlowerLib " FLOWER_VERSION ;


char const * 
flower_version_sz()
{
  return s;
}
