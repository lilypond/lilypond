#include "misc.hh"
#include "textdb.hh"
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
	error( "Unknown symbol `" +s+"'\n");
    }
    Symbol sy;			// unreachable
    return sy;
}

Symtable* 
Symtables::operator()(String s) 
{
    return Assoc<String, Symtable*>::operator[](s);
} 

void
Symtables::read(Text_db &symini)
{
     while (!symini.eof()) {
	 Text_record  r(symini++);
	 if (r[0] == "end" )
	     return;
	 assert (r[0] == "table");
	 
	 String tabnam = r[1];
	 Symtable * sp = new Symtable;
	 while (!symini.eof()){
	     r = symini++;
	     if (r[0] == "end")
		 break;
	     
	     if (r.sz() != 6)
		 error("Not enough fields in symbol init");
	     
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
