#include <math.h>
#include "misc.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "dimen.hh"
#include "debug.hh"

static
char direction_char(int y_sign)
{
    char c='#';
    switch(y_sign){
    case -1:
	c = 'd';
	break;
    case 0:
	c = 'h';
	break;
    case 1:
	c = 'u';
	break;
    default:
	assert(false);
    }
    return c;
}

Symbol
Lookup::half_slur_middlepart(Real &dx, int dir)
{
    if (dx >= convert_dimen(400,"pt")) // todo
	error ("halfslur too large");
    int    widx = int(floor(dx / 4.0));
    dx = widx * 4.0;
    if (widx) widx --;
    else {
	WARN <<  "slur too narrow\n";
    }

    Symbol s;
    
    s.dim.y = Interval(min(0,0), max(0,0));
    s.dim.x = Interval(0,dx);

    String f =  String("\\hslurchar");
    f += direction_char(0);

    int idx = widx;
    if (dir < 0)
	idx += 128;


    f+=String( "{" ) + idx + "}";
    s.tex = f;
    Atom a(s);
    a.translate(Offset(dx/2,0));
    s.tex = a.TeXstring();

    return s;
}
Symbol
Lookup::half_slur(int dy, Real &dx, int dir, int xpart)
{
    if (!xpart)
	return half_slur_middlepart(dx, dir);

    int widx;
    	 	
    if (dx >= convert_dimen(96,"pt")) {
	WARN << "Slur half too wide.";
	dx =  convert_dimen(96,"pt");
    }
    
    widx = int(rint(dx/12.0));
    dx = widx*12.0;
    if (widx)
	widx --;
    else {
	WARN <<  "slur too narrow\n";
    }
	
    Symbol s;
    s.dim.x = Interval(0,dx);
    s.dim.y = Interval(min(0,dy), max(0,dy));


    String f = String("\\hslurchar");

    f+= direction_char(dir);

    int hidx = dy;
    if (hidx <0)
	hidx = -hidx;
    hidx --;
    int idx =-1;
    

    idx = widx * 16 + hidx;
    if (xpart < 0)
	idx += 128;
    
    f+=String( "{" ) + idx + "}";

    
    s.tex = f;
 
    return s;
}

Symbol
Lookup::slur (int dy , Real &dx, int dir)
{				// ugh. assuming pt here.
    assert(dx >=0);
    int y_sign = sign(dy);

    bool large = dy > 16;

    if (y_sign) {
	large |= dx>= convert_dimen(4*16, "pt");
    } else
	large |= dx>= convert_dimen(4*54, "pt");
    
    if (large) {
	return big_slur(dy, dx, dir);
    }
    
    int widx = int(floor(dx/4.0)); // slurs better too small..
    dx = 4.0 * widx;
    if (widx)
	widx --;
    else {
	WARN <<  "slur too narrow\n";
    }

    int hidx = dy;
    if (hidx <0)
	hidx = -hidx;
    hidx --; 
    if (hidx > 16)
	error("slur to steep");
    
    
    Symbol s;
    s.dim.x = Interval(0,dx);
    s.dim.y = Interval(min(0,dy), max(0,dy));

    String f = String("\\slurchar") + direction_char(y_sign);

    int idx=-1;
    if (y_sign) {	
	idx = hidx * 16 + widx;
	if (dir < 0)
	    idx += 128;
    } else {
	if (dx >= convert_dimen(4*54, "pt"))
	    error("slur too wide");
	idx = widx;
	if (dir < 0)
	    idx += 54;		
    }
    
    f+=String( "{" ) + idx + "}";
    s.tex = f;

    Atom a(s);
    a.translate(Offset(dx/2,0));
    s.dim = a.extent();
    s.tex = a.TeXstring();
    return s;    
}

Symbol
Lookup::big_slur(int dy , Real &dx, int dir)
{
    assert(dx >= convert_dimen(24,"pt"));
    Real slur_extra =abs(dy)  /2.0 + 2; 
    int l_dy = int(Real (dy)/2 + slur_extra*dir);
    int r_dy =  dy - l_dy;
    
    Real left_wid = dx/4.0;
    Real right_wid = left_wid;

    Atom l = half_slur(l_dy, left_wid, dir, -1);
    Atom r = half_slur(r_dy, right_wid, dir, 1);
    Real mid_wid = dx - left_wid - right_wid;

    Atom m = half_slur(0, mid_wid, dir, 0);

    Molecule mol;
    mol.add(l);
    Atom a(m);
    a.translate(Offset(0,slur_extra * internote()));
    mol.add_right(m);
    mol.add_right(r);
    mol.translate(Offset(0, l_dy * internote()));
    Symbol s;
    s.tex = mol.TeXstring();
    s.dim = mol.extent();
    return s;
}


