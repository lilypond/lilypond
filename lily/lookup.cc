/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  TODO
  This doth suck. We should have PS output, and read spacing info from TFMs
  
  Glissando, bracket
  
*/

#include "lookup.hh"
#include "debug.hh"
#include "symtable.hh"
#include "dimen.hh"
#include "tex.hh"
#include "scalar.hh"
#include "paper-def.hh"

Lookup::Lookup()
{
    paper_l_ = 0;
    texsetting = "\\unknowntexsetting";
    symtables_ = new Symtables;
}

Lookup::Lookup (Lookup const &s)
{
    paper_l_ = s.paper_l_;
    texsetting = s.texsetting;
    symtables_ = new Symtables (*s.symtables_);
}
Lookup::~Lookup()
{
    delete symtables_;
}

void
Lookup::add (String s, Symtable*p)
{
    symtables_->add (s, p);
}

void
Lookup::print()const
{
    #ifndef NPRINT
    DOUT << "Lookup: " << texsetting << " {\n";
    symtables_->print();
    DOUT << "}\n";
    #endif
}

Symbol
Lookup::text (String style, String text, int dir) const
{
    Array<String> a;
 
    a.push (text);
    Symbol tsym =  (*symtables_)("style")->lookup (style);
    a[0] = substitute_args (tsym.tex,a);

    Symbol s = (*symtables_)("align")->lookup (dir);
    s.tex = substitute_args (s.tex,a);
    s.dim = tsym.dim;
    return s;
}



Symbol
Lookup::ball (int j) const
{
    if (j > 4)
	j = 4;

    Symtable * st = (*symtables_)("balls");
    return st->lookup (String (j));
}

Symbol
Lookup::rest (int j, bool o) const
{
    return (*symtables_)("rests")->lookup (String (j) + (o ? "o" : ""));
}

Symbol
Lookup::fill (Box b) const
{
    Symbol s ((*symtables_)("param")->lookup ("fill"));
    s.dim = b;
    return s;
}

Symbol
Lookup::accidental (int j) const
{
    return (*symtables_)("accidentals")->lookup (String (j));
}


Symbol
Lookup::bar (String s, Real h) const
{
    Array<String> a;
    a.push (print_dimen (h));
    Symbol ret=(*symtables_)("bars")->lookup (s);;
    ret.tex = substitute_args (ret.tex, a);
    ret.dim.y() = Interval (0, h);
    return ret;
}

Symbol
Lookup::script (String s) const
{
    return (*symtables_)("scripts")->lookup (s);
}

Symbol
Lookup::dynamic (String s) const
{
    return (*symtables_)("dynamics")->lookup (s);
}

Symbol
Lookup::clef (String s) const
{
    return (*symtables_)("clefs")->lookup (s);
}
 
Symbol
Lookup::dots (int j) const
{
    if (j>3) {
	j = 3;
	warning ("max 3 dots");	// todo
    }
    return (*symtables_)("dots")->lookup (j);
}

Symbol
Lookup::flag (int j) const
{
    return (*symtables_)("flags")->lookup (j);
}

Symbol
Lookup::streepjes (int i) const
{
    assert (i);
    
    int arg;
    String idx;
    
    if (i < 0) {
	idx = "botlines";
	arg = -i;
    } else {
	arg = i;
	idx = "toplines";
    }
    Symbol ret = (*symtables_)("streepjes")->lookup (idx);
    
    Array<String> a;
    a.push (arg);
    ret.tex = substitute_args (ret.tex, a);

    return ret;
}

Symbol
Lookup::hairpin (Real &wid, bool decresc) const
{
    int idx = int (rint (wid / 6 PT));
    if (!idx) idx ++;
    wid = idx*6 PT;
    String idxstr = (decresc)? "decrescendosym" : "crescendosym";
    Symbol ret=(*symtables_)("param")->lookup (idxstr);
       
    Array<String> a;
    a.push (idx);
    ret.tex = substitute_args (ret.tex, a);
    ret.dim.x() = Interval (0,wid);
    return ret;
}

Symbol
Lookup::linestaff (int lines, Real wid) const
{
    Real internote_f = paper_l_ ->internote_f();
    Symbol s;
    Real dy = (lines >0) ? (lines-1)*internote_f : 0;
    s.dim = Box (Interval (0,wid), Interval (0,dy));

    Array<String> a;
    a.push (lines);
    a.push (print_dimen (wid));

    s.tex = (*symtables_)("param")->lookup ("linestaf").tex;
    s.tex = substitute_args (s.tex, a);

    return s;
}


Symbol
Lookup::meter (Array<Scalar> a) const
{
    Symbol s;
    s.dim.x() = Interval (0 PT, 10 PT);
    s.dim.y() = Interval (0, 20 PT);	// todo
    String src = (*symtables_)("param")->lookup ("meter").tex;
    s.tex = substitute_args (src,a);
    return s;    
}


Symbol
Lookup::stem (Real y1,Real y2) const
{
    if (y1 > y2) {
	Real t = y1;
	y1 = y2;
	y2 = t;
    }
    Symbol s;
    
    s.dim.x() = Interval (0,0);
    s.dim.y() = Interval (y1,y2);
    
    Array<String> a;
    a.push (print_dimen (y1));
    a.push (print_dimen (y2));
	
    String src = (*symtables_)("param")->lookup ("stem").tex;
    s.tex = substitute_args (src,a);
    return s;
}

/*
  should be handled via TeX code and Lookup::bar()
 */
Symbol
Lookup::vbrace (Real &y) const
{
    if (y < 2* 20 PT) {
	warning ( "piano brace too small (" + print_dimen (y)+ ")");
	y = 2*20 PT;
    }
    if (y > 67 * 2 PT) {
	warning ( "piano brace too big (" + print_dimen (y)+ ")");	
	y = 67 *2 PT;
    }
    
    int idx = int (rint ((y/2.0 - 20) + 148));
    
    Symbol s = (*symtables_)("param")->lookup ("brace");
    {
	Array<String> a;
	a.push (idx);
	s.tex = substitute_args (s.tex,a);
	s.dim.y() = Interval (0,y);
    }
    {
	Array<String> a;
	a.push (print_dimen (y/2));
	a.push (print_dimen (0));
	a.push (s.tex);
	s.tex = substitute_args ("\\placebox{%}{%}{%}", a);
    }

	
    return s;
}
