#include "misc.hh"
#include "dimen.hh"
#include "debug.hh"
#include "real.hh"
#include "symbol.hh"
#include "assoc.hh"
#include "symtable.hh"





Symbol 
Symtable::lookup(String s) const
{
    if (elt_query(s))
	return (*this)[s];
    else {
	 Symbol unknown;
	WARN<<"Unknown symbol " << s <<'\n';
	return unknown;
    }
}

Symtable* 
Symtables::operator()(String s) 
{
    if (!done_reading){	// read on demand
	*mlog << '(' << fname ;
	read();
	done_reading = true;
	*mlog << ")\n";
    }
    return Assoc<String, Symtable*>::operator[](s);
} 

void
Symtables::read()
{
     Text_db symini(fname);
     while (!symini.eof()) {
	 Text_record  r(  symini++);
	 assert (r[0] == "table");
	 
	 String tabnam = r[1];
	 Symtable * sp = new Symtable;
	 while (!symini.eof()){
	     r = symini++;
	     if (r[0] == "end")
		 break;
	     
	     assert(r.sz() == 6);
	     int i=0;
	     String id=r[i++];
	     String tex=r[i++];
	     svec<Real> dims;
	     for (int j=0; j < 4; j++)
		 dims.add( parse_dimen(r[i++]));
	     
	     Symbol s(tex, Box(dims));
	     (*sp)[id] = s;
	 }
	 (*this)[tabnam] = sp;	 	 
     }
}



