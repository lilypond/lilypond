/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>

  TODO
  This doth suck. We should have PS output, and read spacing info from TFMs

  Glissando, 

*/

#include "lookup.hh"
#include "debug.hh"
#include "symtable.hh"
#include "dimen.hh"
#include "tex.hh"
#include "scalar.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "main.hh"

Lookup::Lookup()
{
  paper_l_ = 0;
  texsetting = "\\unknowntexsetting";
  symtables_p_ = new Symtables;
}

Lookup::Lookup (Lookup const &s)
{
  paper_l_ = s.paper_l_;
  texsetting = s.texsetting;
  symtables_p_ = new Symtables (*s.symtables_p_);
}
Lookup::~Lookup()
{
  delete symtables_p_;
}

void
Lookup::add (String s, Symtable*p)
{
  symtables_p_->add (s, p);
}

void
Lookup::print() const
{
#ifndef NPRINT
  DOUT << "Lookup: " << texsetting << " {\n";
  symtables_p_->print();
  DOUT << "}\n";
#endif
}

Atom
Lookup::text (String style, String text, int dir) const
{
  Array<String> a;

  a.push (text);
  Atom tsym =  (*symtables_p_)("style")->lookup (style);
  a[0] = substitute_args (tsym.tex_,a);

  Atom s = (*symtables_p_)("align")->lookup (dir);
  s.tex_ = substitute_args (s.tex_,a);
  s.dim_ = tsym.dim_;
  return s;
}



Atom
Lookup::ball (int j) const
{
  if (j > 2)
    j = 2;

  Symtable * st = (*symtables_p_)("balls");
  return st->lookup (String (j));
}

Atom
Lookup::rest (int j, bool o) const
{
  return (*symtables_p_)("rests")->lookup (String (j) + (o ? "o" : ""));
}

Atom
Lookup::fill (Box b) const
{
  Atom s ((*symtables_p_)("param")->lookup ("fill"));
  s.dim_ = b;
  return s;
}

Atom
Lookup::accidental (int j) const
{
  return (*symtables_p_)("accidentals")->lookup (String (j));
}


Atom
Lookup::bar (String s, Real h) const
{
  Array<String> a;
  a.push (print_dimen (h));
  Atom ret=(*symtables_p_)("bars")->lookup (s);
  ret.tex_ = substitute_args (ret.tex_, a);
  ret.dim_.y() = Interval (-h/2, h/2);
  return ret;
}

Atom
Lookup::script (String s) const
{
  return (*symtables_p_)("scripts")->lookup (s);
}

Atom
Lookup::dynamic (String s) const
{
  return (*symtables_p_)("dynamics")->lookup (s);
}

Atom
Lookup::clef (String s) const
{
  return (*symtables_p_)("clefs")->lookup (s);
}

Atom
Lookup::dots () const
{
  return (*symtables_p_)("dots")->lookup ("dot");
}

Atom
Lookup::flag (int j, Direction d) const
{
  char c = (d == UP) ? 'u' : 'd';
  return (*symtables_p_)("flags")->lookup (c + String (j));
}

Atom
Lookup::slur (Array<Offset> controls) const
{
  assert (controls.size () == 8);

  String ps = "\\embeddedps{\n";
  
  Real dx = controls[3].x () - controls[0].x ();
  Real dy = controls[3].y () - controls[0].y ();

  for (int i = 5; i < 8; i++)
    ps += String_convert::double_str (controls[i].x ()) + " "
      + String_convert::double_str (controls[i].y ()) + " ";

  ps += String_convert::double_str (controls[4].x ()) + " "
    + String_convert::double_str (controls[4].y ()) + " ";

  for (int i = 1; i < 4; i++)
    ps += String_convert::double_str (controls[i].x ()) + " "
      + String_convert::double_str (controls[i].y ()) + " ";

  ps += String_convert::double_str (controls[0].x ()) + " "
    + String_convert::double_str (controls[0].y ()) + " ";

  ps += " draw_slur}";

  Atom s;
  s.tex_ = ps;
  
  s.dim_[X_AXIS] = Interval (0, dx);
  s.dim_[Y_AXIS] = Interval (0 <? dy,  0 >? dy);
  return s;
}

Atom
Lookup::streepje (int type) const
{
  if (type > 2)
    type = 2;

  Symtable * st = (*symtables_p_)("balls");
  
  return st->lookup (String (type) + 'l');
}

Atom
Lookup::hairpin (Real &wid, bool decresc, bool continued) const
{
  String embed;
  Atom ret;  
  Real height = paper_l_->get_var ("barsize") / 6;
  embed = "\\embeddedps{\n" ;
  embed += String (wid) + " " 
	+ String (height) + " " 
    + String (continued ? height/2 : 0) + 
    + " draw_"  + String(decresc ? "de" : "") + "cresc}\n";
  ret.tex_ = embed;


  ret.dim_.x () = Interval (0,wid);
  ret.dim_.y () = Interval (-2*height,2*height);

  return ret;
}

Atom
Lookup::meter (Array<Scalar> a) const
{
  Atom s((*symtables_p_)("param")->lookup ("meter"));
  s.tex_ = substitute_args (s.tex_,a);
  return s;
}


Atom
Lookup::stem (Real y1,Real y2) const
{
  if (y1 > y2)
    {
      Real t = y1;
      y1 = y2;
      y2 = t;
    }
  Atom s;

  s.dim_.x() = Interval (0,0);
  s.dim_.y() = Interval (y1,y2);

  Array<String> a;
  a.push (print_dimen (y1));
  a.push (print_dimen (y2));

  String src = (*symtables_p_)("param")->lookup ("stem").tex_;
  s.tex_ = substitute_args (src,a);
  return s;
}

/*
  should be handled via Tex_ code and Lookup::bar()
 */
Atom
Lookup::vbrace (Real &y) const
{
  Atom brace = (*symtables_p_)("param")->lookup ("brace");
  Interval ydims = brace.dim_[Y_AXIS];
  Real min_y = ydims[LEFT];
  Real max_y = ydims[RIGHT];
  Real step = 1.0 PT;
 
  if (y < min_y)
    {
      warning (_("piano brace too small (") + print_dimen (y)+ ")");
      y = min_y;
    }
  if (y > max_y)
    {
      warning (_("piano brace too big (") + print_dimen (y)+ ")");
      y = max_y;
    }

  
  int idx = int (rint ((y- min_y)/step)) + 1;
  
  {
    Array<String> a;
    a.push (idx);
    brace.tex_ = substitute_args (brace.tex_,a);
    brace.dim_[Y_AXIS] = Interval (-y/2,y/2);
  }

  return brace;
}

Atom
Lookup::vbracket (Real &y) const
{
  Atom psbracket;
  psbracket.tex_ = String ("\\embeddedps{ ") + y + " draw_bracket}";
  psbracket.dim_[Y_AXIS] = Interval (-y/2,y/2);
  psbracket.dim_[X_AXIS] = Interval (0,4 PT);
  return psbracket;
  Atom bracket = (*symtables_p_)("param")->lookup ("bracket");
  Interval ydims = bracket.dim_[Y_AXIS];

  
  Real min_y = ydims[LEFT];
  Real max_y = ydims[RIGHT];
  Real step = 1.0 PT;
 
  if (y < min_y)
    {
      warning (_("bracket too small (") + print_dimen (y)+ ")");
      y = min_y;
    }
  if (y > max_y)
    {
      warning (_("bracket too big (") + print_dimen (y)+ ")");
      y = max_y;
    }

  
  int idx = int (rint ((y- min_y)/step)) + 1;
  
  {
    Array<String> a;
    a.push (idx);
    bracket.tex_ = substitute_args (bracket.tex_,a);
    bracket.dim_[Y_AXIS] = Interval (-y/2,y/2);
  }

  return bracket;
}

