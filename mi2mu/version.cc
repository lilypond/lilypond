//
// version.cc -- implement inexpensive versioning
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "mi2mu.hh"
#include "fversion.hh"
#include "version.hh"

// should simply have Root class...
String
version_str()
{
	return String ( "This is mi2mu " ) + VERSIONSTR 
		+ "/FlowerLib " + FVERSIONSTR
		+ " of " +  __DATE__ + " " + __TIME__;
}

