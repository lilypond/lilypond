#include "proto.hh"
#include "string.hh"

const char * midi2ly_version_sz();

// should simply have Root class...
String
midi2ly_version_str()
{
  return  String (midi2ly_version_sz ())
	+ " " + _("of") + " " +  __DATE__ + " " + __TIME__;
}

