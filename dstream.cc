// implementation of debug/TeX stream.
#include <fstream.h>

#include "dstream.hh"
#include "string.hh"



dstream mtor(cout);

///
dstream &
dstream::operator<<(String s)
{       
    for (const char *cp = s  ; *cp; cp++)
	switch(*cp) 
	    {
	    case '{':
	    case '[':
	    case '(': indentlvl += INDTAB;
		*os << *cp;		
		break;
		
	    case ')':
	    case ']':
	    case '}':
		indentlvl -= INDTAB;
		*os << *cp		;
		
		if  (indentlvl<0) indentlvl = 0;
		break;
		
	    case '\n':
		*os << '\n' << String (' ', indentlvl) << flush;
		break;	      
	    default:
		*os << *cp;
		break;
	    }
    return *this;
    
}

/** only output possibility. Delegates all conversion to String class.
 */

