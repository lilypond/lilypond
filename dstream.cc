#include <fstream.h>

#include "dstream.hh"
#include "string.hh"
#include "textdb.hh"


/*
  should use Regexp library.
  */
static String
strip_pretty(String pret)
{
    String cl(pret.left(pret.pos('(')-1));
    int l = cl.lastPos(' ');
    cl = cl.right(cl.len() -l);
    return cl;
}

static String
strip_member(String pret)
{
    String cl(pret.left(pret.lastPos(':')-2));
    return cl;
}

Dstream&
Dstream::identify_as(String name)
{
    String mem(strip_pretty(name));
    String cl(strip_member(mem));
    
    if(!silent.elt_query(cl))
	silent[cl] = false;
    local_silence = silent[cl];
    if (classname != cl && !local_silence) {
	classname=cl;
	*os << "[" << classname << ":]";
    }
    return *this;
}

void
Dstream::switch_output(String name,bool b)
{
    silent[name] = b;
}

///
Dstream &
Dstream::operator<<(String s)
{
    if (local_silence)
	return *this;
    
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

Dstream::Dstream(ostream &r, const char * cfg_nm )
{
    os = &r;
    indentlvl = 0;
    
    const char * fn =cfg_nm ? cfg_nm : ".dstreamrc";
    {
	ifstream ifs(fn);	// can't open
	if (!ifs)
	    return;
    }
    cerr << "(" << fn;
    Text_db cfg(fn);
    while (! cfg.eof()){	     
	 Text_record  r(  cfg++);
	 assert(r.sz() == 2);
	 silent[r[0]] = r[1].to_bool();
    }
    cerr <<")";
}


