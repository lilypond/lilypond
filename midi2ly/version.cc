#include "proto.hh"
#include "string.hh"

const char * mi2mu_version_sz();

// should simply have Root class...
String
mi2mu_version_str()
{
  return  String (mi2mu_version_sz ())
	+ " " + _("of") + " " +  __DATE__ + " " + __TIME__;
}

