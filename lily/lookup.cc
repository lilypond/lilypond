/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  TODO
  This doth suck. We should have PS output, and read spacing info from AFMs

  Glissando, 

*/

#include "lookup.hh"
#include "debug.hh"
#include "symtable.hh"
#include "dimension.hh"
#include "tex.hh"
#include "scalar.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "main.hh"

Lookup::Lookup()
{
  paper_l_ = 0;
  symtables_p_ = new Symtables;
  afm_p_ =0;
}

Lookup::Lookup (Lookup const &s)
{
  font_ = s.font_;
  font_path_ = s.font_path_;
  paper_l_ = s.paper_l_;
  symtables_p_ = new Symtables (*s.symtables_p_);
  afm_p_ = 0;
}

Lookup::~Lookup()
{
  delete afm_p_;
  delete symtables_p_;
}

Atom
Lookup::afm_find (String s) const
{
  if (!afm_p_)
    {
      *mlog << "[" << font_path_;
      ((Lookup*)this)->afm_p_ = new Adobe_font_metric (read_afm (font_path_));
      *mlog << "]" << flush ;
      DOUT << this->afm_p_->str ();
    }
  Adobe_font_char_metric m = afm_p_->find_char (s);

  Atom a;
  a.tex_ = String_convert::form_str ("\\char%d", m.code ());
  a.dim_ = m.B_;
  a.dim_[X_AXIS] *= 1 / 1000.0;
  a.dim_[Y_AXIS] *= 1 / 1000.0;
  a.font_ = font_;
  return a;
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
  DOUT << "Lookup {\n";
  symtables_p_->print();
  DOUT << "}\n";
#endif
}

Atom
Lookup::text (String style, String text) const
{
  Array<String> a;

  a.push (text);
  Atom tsym =  (*symtables_p_)("style")->lookup (style);
  tsym.tex_ = substitute_args (tsym.tex_,a);
  tsym.font_ = font_;

  return tsym;
}



Atom
Lookup::ball (int j) const
{
  if (j > 2)
    j = 2;

  Atom s = afm_find (String ("balls") + String ("-") + to_str (j));
  return s;
}

Atom
Lookup::rest (int j, bool o) const
{
  Atom s =afm_find (String ("rests")
		    + String ("-") + to_str (j) + (o ? "o" : ""));

  return s;
}

Atom
Lookup::fill (Box b) const
{
  Atom s;
  s.tex_ = "";
  s.dim_ = b;

  return s;
}

Atom
Lookup::accidental (int j) const
{
  Atom s= afm_find (String ("accidentals") + String ("-") + to_str (j));
  return s;
}


Atom
Lookup::bar (String s, Real h) const
{
  Array<String> a;
  a.push (print_dimen (h));
  Atom ret=(*symtables_p_)("bars")->lookup (s);
  ret.tex_ = substitute_args (ret.tex_, a);
  ret.dim_.y() = Interval (-h/2, h/2);
  ret.font_ = font_;
  return ret;
}

Atom
Lookup::script (String st) const
{
  Atom s= afm_find (String ("scripts") + String ("-") + st);

  return s;
}

Atom
Lookup::dynamic (String st) const
{
  return (*symtables_p_) ("dynamics")->lookup (st);
}

Atom
Lookup::clef (String st) const
{
  Atom s=afm_find (String ("clefs") + String ("-") + st);

  return s;
}

Atom
Lookup::dots () const
{
  Atom s=afm_find (String ("dots") + String ("-") + String("dot"));
  
    return s;
}

Atom
Lookup::flag (int j, Direction d) const
{
  char c = (d == UP) ? 'u' : 'd';
  Atom s=afm_find (String ("flags") + String ("-") + to_str (c) + to_str (j));

  return s;
}

Atom
Lookup::dashed_slur (Array<Offset> controls, Real thick, Real dash) const
{
  assert (controls.size () == 8);

  String ps = "\\embeddedps{\n";
  
  Real dx = controls[3].x () - controls[0].x ();
  Real dy = controls[3].y () - controls[0].y ();

  for (int i = 1; i < 4; i++)
    ps += String_convert::double_str (controls[i].x ()) + " "
      + String_convert::double_str (controls[i].y ()) + " ";

  ps += String_convert::double_str (controls[0].x ()) + " "
    + String_convert::double_str (controls[0].y ()) + " ";

  ps += String_convert::double_str (thick) + " ";
  Real on = dash > 1? thick * dash - thick : 0;
  Real off = 2 * thick;
  ps += "[" + String_convert::double_str (on) + " ";
  ps += String_convert::double_str (off) + "] ";
  ps += String_convert::int_str (0) + " ";
  ps += " draw_dashed_slur}";

  Atom s;
  s.tex_ = ps;
  
  s.dim_[X_AXIS] = Interval (0, dx);
  s.dim_[Y_AXIS] = Interval (0 <? dy,  0 >? dy);
  s.font_ = font_;
  return s;
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
  s.font_ = font_;
  return s;

}

Atom
Lookup::streepje (int type) const
{
  if (type > 2)
    type = 2;

  return  afm_find ("balls" + String ("-") +to_str (type) + "l");
}

Atom
Lookup::hairpin (Real width, bool decresc, bool continued) const
{
  String embed;
  Atom ret;  
  Real height = paper_l_->staffheight_f () / 6;
  embed = "\\embeddedps{\n" ;
  embed += to_str (width) + " " 
	+ to_str (height) + " " 
    + to_str (continued ? height/2 : 0) + 
    + " draw_"  + String(decresc ? "de" : "") + "cresc}\n";
  ret.tex_ = embed;


  ret.dim_.x () = Interval (0, width);
  ret.dim_.y () = Interval (-2*height, 2*height);
  ret.font_ = font_;
  return ret;
}

Atom
Lookup::time_signature (Array<Scalar> a) const
{
  Atom s((*symtables_p_)("param")->lookup ("time_signature"));
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

  
  Real stem_width = paper_l_->get_var ("stemthickness");
  a.push (print_dimen (-stem_width /2));
  a.push (print_dimen (stem_width));
  a.push (print_dimen (y2));
  a.push (print_dimen (-y1));

  String src = "\\kern %\\vrule width % height % depth %";
  s.tex_ = substitute_args (src,a);
  s.font_ = font_;
  return s;
}

/*
  should be handled via Tex_ code and Lookup::bar()
 */
Atom
Lookup::vbrace (Real &y) const
{
  Atom brace = (*symtables_p_) ("param")->lookup ( "brace");
  Interval ydims = brace.dim_[Y_AXIS];
  Real min_y = ydims[LEFT];
  Real max_y = ydims[RIGHT];
  Real step = 1.0 PT;
 
  if (y < min_y)
    {
      warning (_ ("piano brace") 
	+ " " + _ ("too small") +  " (" + print_dimen (y) + ")");
      y = min_y;
    }
  if (y > max_y)
    {
      warning (_ ("piano brace")
       + " " + _ ("too big") + " (" + print_dimen (y) + ")");
      y = max_y;
    }

  
  int idx = int (rint ((y- min_y)/step)) + 1;
  
  {
    Array<String> a;
    a.push (to_str (idx));
    brace.tex_ = substitute_args (brace.tex_,a);
    brace.dim_[Y_AXIS] = Interval (-y/2,y/2);
  }

  brace.font_ = font_;

  return brace;
}

Atom
Lookup::vbracket (Real &y) const
{
  Atom psbracket;
  Real min_y = paper_l_->staffheight_f ();
  if (y < min_y)
    {
      warning (_ ("bracket")
	+ " " + _ ("too small") +  "(" + print_dimen (y) + ")");
//      y = min_y;
    }
  psbracket.tex_ = String ("\\embeddedps{ ") + to_str (y) + " draw_bracket}";
  psbracket.dim_[Y_AXIS] = Interval (-y/2,y/2);
  psbracket.dim_[X_AXIS] = Interval (0,4 PT);
  return psbracket;
#if 0
  Atom bracket = afm_find (String ("param") + "bracket");
  Interval ydims = bracket.dim_[Y_AXIS];

  Real min_y = ydims[LEFT];
  Real max_y = ydims[RIGHT];
  Real step = 1.0 PT;
 
  if (y < min_y)
    {
      warning (_ ("bracket")
	+ " " + _ ("too small") +  " (" + print_dimen (y) + ")");
      y = min_y;
    }
  if (y > max_y)
    {
      warning (_ ("bracket")
       + " " + _ ("too big") + " (" + print_dimen (y) + ")");
      y = max_y;
    }

  
  int idx = int (rint ((y- min_y)/step)) + 1;
  
  {
    Array<String> a;
    a.push (to_str (idx));
    bracket.tex_ = substitute_args (bracket.tex_,a);
    bracket.dim_[Y_AXIS] = Interval (-y/2,y/2);
  }

  return bracket;
#endif
}

