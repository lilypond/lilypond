// debug_stream

#ifndef DSTREAM_HH
#define DSTREAM_HH

#include "string.hh"

const char eol= '\n';

template<class K,class V>
struct Assoc;

/// debug stream
class Dstream
{
    ostream *os;
    int indentlvl;
    bool local_silence;
    String classname;

    Assoc<String, bool> *silent;
public:

    bool silence(String);
    
    Dstream(ostream *r, const char  * rcfile);
    /**
      if rcfile == 0, then do not read any rc file
      */
      
    virtual ~Dstream();
    Dstream &identify_as(String s);
    
    Dstream &operator << (String s);
};
 /**
   a class for providing debug output of nested structures,
   with indents according to \{\}()[].

   One can turn on and off specific messages using the Assoc silent.
   This can be done automatically:

   #define DEBUG  dstream_.identify_as(__PRETTY_FUNCTION__) 

   DEBUG << "a message\n";
   
   Init for the class names which should be silent can be given in a rc file. 
   
  */
#endif

