#include <fstream.h>
#include "tex.hh"
#include "tstream.hh"
#include "debug.hh"

Tex_stream::Tex_stream(String filename) 
{
    
    os = new ofstream(filename);
    if (!*os)
	error("can't open " + filename);
    nest_level = 0;
    outputting_comment=false;
}

Tex_stream::~Tex_stream()
{
    assert(nest_level == 0);
    delete os;
}

// print string. don't forget indent.
Tex_stream &
Tex_stream::operator<<(String s)
{       
    for (const char *cp = s; *cp; cp++) {
	if (outputting_comment) {
	    *os << *cp;
	    if (*cp == '\n') {
		outputting_comment=false;

	    }
	    continue;
	}
	switch(*cp) 
	    {
	    case '%':
		outputting_comment = true;
		*os << *cp;
		break;
	    case '{':
		nest_level++;
		*os << *cp;		
		break;
	    case '}':
		nest_level--;		
		*os << *cp;
		assert (nest_level >= 0);
		/* FALL THROUGH */
		
	    case '\n':
		*os << "%\n";
		*os << String(' ', nest_level);
		break;	      
	    default:
		*os << *cp;
		break;
	    }
    }
    return *this;
}


/****************************************************************/
