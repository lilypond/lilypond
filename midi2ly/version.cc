#include "proto.hh"
#include "string.hh"

const char * midi2ly_version_sz();

String
midi2ly_version_str()
{
  return  String (midi2ly_version_sz ());
}

