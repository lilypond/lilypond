#include "mi2mu.hh"

const char * mi2mu_version_sz();

// should simply have Root class...
String
mi2mu_version_str()
{
	return String ( "This is " ) + mi2mu_version_sz()
	    + "/" + flower_version_sz()
		+ " of " +  __DATE__ + " " + __TIME__;
}

