

#include <fstream.h>
#include "assoc.hh"
#include "dstream.hh"
#include "scalar.hh"
#include "text-db.hh"
#include "string-convert.hh"

/// indent of each level 
const INDTAB = 2;

/*
  should use Regexp library.
  */
static String
strip_pretty(String pretty_str)
{
    int i = pretty_str.index_i('(');
    if (i>=0)
	pretty_str = pretty_str.left_str(i);
    
    int l = pretty_str.index_last_i(' '); // strip until last ' '
    if (l>=0)
	pretty_str = pretty_str.nomid_str(0,l+1);
    return pretty_str;
}

static String
strip_member(String pret)
{
    int l=pret.index_last_i(':')-1;
    if (l>=0)
	pret = pret.left_str(l );
    return pret;
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
	if (!(*silent)["Dstream"])
	    *os << "[" << classname << ":]"; // messy.
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

/** Output a string via the Dstream. This is the only output
 interface. It delegates all conversion to String class.  */
Dstream &
Dstream::operator<<(String s)
{
    output(s);
    return *this;
}

Dstream &
Dstream::operator<<(void const *v_l)
{
    output(String_convert::pointer_str(v_l));
    return *this;
}

Dstream &
Dstream::operator<<(char const *ch_l)
{
    output(ch_l);
    return *this;
}

void
Dstream::output(String s)
{
    if (local_silence|| !os)
	return ;
    
    for (char const *cp = s  ; *cp; cp++)
	switch(*cp) {
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
    return ;    
}


Dstream::Dstream(ostream *r, char const * cfg_nm )
{
    os = r;
    silent = new Assoc<String,bool>;
    indentlvl = 0;
    if (!os)
	return;
    
    char const * fn =cfg_nm ? cfg_nm : ".dstreamrc";
    {
	ifstream ifs(fn);	// can't open
	if (!ifs)
	    return;
    }

    Text_db cfg(fn);
    while (! cfg.eof()){	     
	 Text_record  r(  cfg++);
	 if (r.size() != 2) {
	     r.message("not enough fields in Dstream init.");
	     continue;
	 }
	 (*silent)[r[0]] = (bool)(int)(Scalar(r[1]));
    }

}


Dstream::~Dstream()
{    
    delete silent;
    assert(!indentlvl) ;
}
