#include "misc.hh"
#include "debug.hh"
#include "real.hh"
#include "symbol.hh"
#include "assoc.hh"
#include "symtable.hh"
#include "const.hh"


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
		 dims.add( r[i++].fvalue() *1.0/CM_TO_PT);
	     
	     Symbol s(tex, Box(dims));
	     (*sp)[id] = s;
	 }
	 (*this)[tabnam] = sp;	 	 
     }
}

Symtables the_sym_tables("symbol.ini");


const Symbol*
Symbol::find_ball(int i)
{
    int j = intlog2(i)+1;
    if (j > 4) j = 4;
    Symtable * st = the_sym_tables("balls");
    return &(*st)[String(j)];

}

const Symbol*
Symbol::find_rest(int i)
{
    int j = intlog2(i)+1;
    return &(*the_sym_tables("rests"))[String(j)];
}
const Symbol*
Symbol::find_bar(String s)
{
    return &(*the_sym_tables("bars"))[s];  
}
/****************************************************************/
// bare bones.

struct Linestaf_symbol : Stretchable_symbol {
    int lines;
    String operator ()(Real w);
    Linestaf_symbol(int n) { lines = n;}
    Interval height(Real) const { return Interval(0,lines*1/CM_TO_PT); }
};



// should be done in TeX
String
Linestaf_symbol::operator()(Real w)
{
    String s;
    s += "\\hbox to 0pt{";
    s+= "\\vbox to 0pt{";
    for (int i=0; i<lines; i++) {
	if (i) s+= "\\vskip1pt";
	s+= "\\hrule width " + String(w* HOR_TO_PT) +"pt";
    }
    s+="\\vss}\\hss}";
    return s;
}

const Stretchable_symbol *
Stretchable_symbol::get_linestaff(int n)
{
    return new Linestaf_symbol(n);
}
