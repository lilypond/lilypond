#include <fstream.h>
#include "assoc.hh"
#include "dstream.hh"
#include "string.hh"
#include "textdb.hh"

/// indent of each level 
const INDTAB = 3;

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
    if (!os)
	return *this;
    
    String mem(strip_pretty(name));
    String cl(strip_member(mem));
    String idx = cl;
    
    if (silent->elt_query(mem))
	idx  = mem;
    else if (silent->elt_query(cl))
	idx = cl;
    else {
	(*silent)[idx] = false;
    }
    local_silence = (*silent)[idx];
    if (classname != idx && !local_silence) {
	classname=idx;
	*os << "[" << classname << ":]";
    }
    return *this;
}

bool
Dstream::silence(String s)
{
    if (!silent->elt_query(s))
	return false;
    return (*silent)[s];
}
///
Dstream &
Dstream::operator<<(String s)
{
    if (local_silence|| !os)
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
		
		assert  (indentlvl>=0) ;
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

Dstream::Dstream(ostream *r, const char * cfg_nm )
{
    os = r;
    silent = new Assoc<String,bool>;
    if (!os)
	return;
    indentlvl = 0;
    
    const char * fn =cfg_nm ? cfg_nm : ".dstreamrc";
    {
	ifstream ifs(fn);	// can't open
	if (!ifs)
	    return;
    }

    Text_db cfg(fn);
    while (! cfg.eof()){	     
	 Text_record  r(  cfg++);
	 assert(r.sz() == 2);
	 (*silent)[r[0]] = r[1].to_bool();
    }

}


Dstream::~Dstream()
{
    delete silent;
}
