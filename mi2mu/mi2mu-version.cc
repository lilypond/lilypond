//
// version.cc -- implement inexpensive versioning
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <stdio.h>
#include "config.hh"


// static char *s = "mi2mu " MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL MY_PATCH_LEVEL " #%d";
//urg
//static char *s = "mi2mu " TOPLEVEL_VERSION " #%d";
static char *s = "mi2mu FIXME " ;


const char * 
mi2mu_version_sz()
{  return s;
}

