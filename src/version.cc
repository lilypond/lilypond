#include "version.hh"

static char *s = "LilyPond version " VERSIONSTR " compiled on "
   __DATE__ " at " __TIME__ " with " COMPILER "\n";

const char *
get_version()
{
   return s;
}
