#include "../out/version.hh"	// urg
#include <stdio.h>

static char *s = "FlowerLib " MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL MY_PATCH_LEVEL " #%d";

static const int build=
#include "../out/.build"
;

const char * 
flower_version_sz()
{
    static char v[1024];
    sprintf(v, s, build);
    return v;
}
