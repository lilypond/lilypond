//
// version.cc -- implement inexpensive versioning
//
// (c) 1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>

#include <stdio.h>
#include "config.h"
#include "version.hh"

#define VERSION_SZ  MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL "." MY_PATCH_LEVEL

static char *s = "midi2ly " VERSION_SZ;


const char * 
midi2ly_version_sz()
{
  return s;
}

