#include "version.hh"
#include "fversion.hh"

static char *s = "LilyPond " VERSIONSTR    "/FlowerLib " FVERSIONSTR
". Compile: "   __DATE__ ", " __TIME__ " (" COMPILER ")\n";

const char *
get_version()
{
   return s;
}
