// debug_stream

#ifndef DSTREAM_HH
#define DSTREAM_HH

#include "string.hh"
#include "assoc.hh"

const char eol= '\n';

/// debug stream
class Dstream
{
    ostream *os;
    int indentlvl;
    Assoc<String, bool> silent;
    bool local_silence;
    String classname;
    /// indent of each level 
    const INDTAB = 3;

public:
    Dstream(ostream &r, const char  * = 0);
    Dstream &identify_as(String s);
    void switch_output(String s,bool);
    Dstream &operator << (String s);
};
 /**
   a class for providing debug output of nested structures,
   with indents according to \{\}()[].

   Using identify_as() one can turn on and off specific messages. Init
   for these can be given in a rc file
   
  */
#endif

