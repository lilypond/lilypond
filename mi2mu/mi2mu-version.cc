//
// version.cc -- implement inexpensive versioning
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <stdio.h>
#include "version.hh"


static char *s = "mi2mu " MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL MY_PATCH_LEVEL " #%d";

static const int build=
#include ".build"
;

const char * 
mi2mu_version_sz()
{
  static char v[1024];
  sprintf(v, s, build);
  return v;
}

