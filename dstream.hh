// debug_stream

#ifndef DSTREAM_HH
#define DSTREAM_HH

#include "string.hh"

const char eol= '\n';

/// debug stream
class dstream
{
    ostream *os;
    int indentlvl;

    /// indent of each level 
    const INDTAB = 3;
public:
    dstream(ostream &r){
	os = &r;
	indentlvl = 0;	
    }
    dstream &operator << (String s);
};
 /**
   a class for providing debug output of nested structures,
   with indents according to \{\}()[]
  */     
#endif
