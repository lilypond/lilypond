/*

  Code to generate beams for TeX
  
  */

  #include <math.h>
#include "symbol.hh"
#include "molecule.hh"
#include "tex.hh"
#include "symtable.hh"
#include "dimen.hh"
#include "lookup.hh"

Symbol
Lookup::beam_element(int sidx, int widx, Real slope)
{
    Symbol bs=(*symtables_)("beamslopes")->lookup("slope");
    
    svec<String> args;
    args.add(sidx);
    args.add(widx);
    bs.tex = substitute_args(bs.tex,args);
    int w = 2 << widx;
    Real width = convert_dimen(w,"pt");
    bs.dim.x = Interval(0,width);
    bs.dim.y = Interval(0,width*slope);
    return bs;
}

// ugh.. hard wired tex-code.
static int
slope_index(Real &s)
{
    assert(ABS(s) < 0.45);
    int i = int(rint(s *  20.0));

    s = i/20.0;
    if (s>0)
	return 6*i +122;
    else
	return -6 * i+ 188;
}

Symbol
Lookup::rule_symbol(Real height, Real width)
{
    Symbol bs=(*symtables_)("beamslopes")->lookup("horizontal");    
    svec<String> args;
    args.add(print_dimen(height));
    args.add(print_dimen(width));
    bs.tex = substitute_args(bs.tex,args);
    bs.dim.x = Interval(0,width);
    bs.dim.y = Interval(0,height);
    return bs;
}

Symbol
Lookup::beam(Real &slope, Real width)
{        
    int sidx = slope_index(slope);
    if (!slope)
	return rule_symbol(convert_dimen(2,"pt"), width);
        
    Real w = width;
    Real elemwidth = convert_dimen(64,"pt");
    int widx = 5;

    Molecule m;
    Real dy=0;
    Real minwid =convert_dimen(2,"pt");
    assert(w > minwid);
    while (w > minwid) {
	while (elemwidth > w) {
	    widx --;
	    elemwidth /= 2.0;
	}
	
	Atom a(beam_element(sidx, widx, slope));
	a.translate(Offset(0, dy));
	m.add_right(a);
	dy += elemwidth*slope;
	w -= elemwidth;
    }

    widx = 0;
    Atom a(beam_element(sidx, widx, slope));
    a.translate(Offset(width -minwid, (width-minwid) * slope));
    m.add(a);
    
    Symbol ret;
    ret.tex = m.TeXstring();
    ret.dim.y = Interval(0,width*slope);
    ret.dim.x = Interval(0,width);
    
    return ret;
}


