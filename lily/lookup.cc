/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  Jan Nieuwenhuizen <janneke@gnu.org>

  TODO
      Read spacing info from AFMs
      Glissando
*/

#include "lookup.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "symtable.hh"
#include "scalar.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "main.hh"

Lookup::Lookup ()
{
  paper_l_ = 0;
  symtables_p_ = new Symtables;
  afm_p_ =0;
}

Lookup::Lookup (Lookup const& s)
{
  font_ = s.font_;
  font_path_ = s.font_path_;
  paper_l_ = s.paper_l_;
  symtables_p_ = new Symtables (*s.symtables_p_);
  afm_p_ = 0;
}

Lookup::Lookup (Symtables const& s)
{
  font_ = s.font_;
  font_path_ = s.font_path_;
  paper_l_ = 0;
  symtables_p_ = new Symtables (s);
  afm_p_ = 0;
}

Lookup::~Lookup ()
{
  delete afm_p_;
  delete symtables_p_;
}

Atom
Lookup::accidental (int j) const
{
  return afm_find (String ("accidentals") + String ("-") + to_str (j));
}

void
Lookup::add (String s, Symtable*p)
{
  symtables_p_->add (s, p);
}

Atom
Lookup::afm_find (String s, String str) const
{
  if (!afm_p_)
    {
      *mlog << "[" << font_path_;
      ( (Lookup*)this)->afm_p_ = new Adobe_font_metric (read_afm (font_path_));
      *mlog << "]" << flush ;
      DOUT << this->afm_p_->str ();
    }
  Adobe_font_char_metric m = afm_p_->find_char (s);

  Atom a;
  if (m.code () < 0)
    return a;
  
  a.dim_ = m.B_;
  a.dim_[X_AXIS] *= 1 / 1000.0;
  a.dim_[Y_AXIS] *= 1 / 1000.0;
  a.str_ = String_convert::form_str (str.ch_C (), m.code ());
  a.font_ = font_;
  return a;
}

Atom
Lookup::ball (int j) const
{
  if (j > 2)
    j = 2;

  return afm_find (String ("balls") + String ("-") + to_str (j));
}

Atom
Lookup::bar (String str, Real h) const
{
  Array<String> a;
  a.push (print_dimen (h));
  Atom s = (*symtables_p_) ("bars")->lookup (str);
  s.str_ = substitute_args (s.str_, a);
  s.dim_.y () = Interval (-h/2, h/2);
  s.font_ = font_;
  return s;
}

String
Lookup::base_output_str () const
{
  assert (paper_l_);
  String str = paper_l_->get_default_output ();

  if (str.empty_b ())
    {
      str = default_outname_base_global;
      int def = paper_l_->get_next_default_count ();
      if (def)
	str += "-" + to_str (def);
    }
  return str;
}

Atom 
Lookup::beam (Real slope, Real width, Real thick) const
{
  Atom a (ps_beam (slope, width, thick));
  Real height = slope * width; 
  Real min_y = (0 <? height) - thick/2;
  Real max_y = (0 >? height) + thick/2;
  
  a.dim_[X_AXIS] = Interval (0, width);
  a.dim_[Y_AXIS] = Interval (min_y, max_y);
  return a;
}

String
Lookup::character_str (int i) const
{
  return to_str (i);
}

Atom
Lookup::clef (String st) const
{
  return afm_find (String ("clefs") + String ("-") + st);
}

Atom
Lookup::dots () const
{
  return afm_find (String ("dots") + String ("-") + String ("dot"));
}

Atom
Lookup::dynamic (String st) const
{
  return (*symtables_p_) ("dynamics")->lookup (st);
}

Atom
Lookup::fill (Box b) const
{
  Atom a;
  a.dim_ = b;
  return a;
}

Atom
Lookup::flag (int j, Direction d) const
{
  char c = (d == UP) ? 'u' : 'd';
  return afm_find (String ("flags") + String ("-") + to_str (c) + to_str (j));
}

void
Lookup::print () const
{
#ifndef NPRINT
  DOUT << "Lookup {\n";
  symtables_p_->print ();
  DOUT << "}\n";
#endif
}

String
Lookup::print_dimen (Real r) const
{
  String s = to_str (r, "%.3f");
  if (s.index_i ("NaN") != -1)
    {
      warning (_ ("NaN"));
      s = "0.0";
    }
  return s;
}

Atom
Lookup::rest (int j, bool o) const
{
   return afm_find (String ("rests")
		    + String ("-") + to_str (j) + (o ? "o" : ""));
}

Atom
Lookup::rule_symbol (Real height, Real width) const
{
  Atom bs= (*symtables_p_) ("param")->lookup ("rule");
  Array<String> args;
  args.push (print_dimen (height));
  args.push (print_dimen (width));
  bs.str_ = substitute_args (bs.str_, args);
  bs.dim_.x () = Interval (0, width);
  bs.dim_.y () = Interval (0, height);
  return bs;
}

Atom
Lookup::script (String str) const
{
  return afm_find (String ("scripts") + String ("-") + str);
}

Atom
Lookup::special_time_signature (String s, Array<Scalar> arr) const
{
  String symbolname = "timesig-"+s+"%/%";
  Atom a (afm_find (substitute_args (symbolname, arr)));
  if (!a.empty ()) 
    return a;
  // Try if the full name was given
  a = afm_find ("timesig-"+s);
  if (!a.empty ()) 
    return a;
  // Resort to default layout with numbers
  return time_signature (arr);
}

static void
substitute_arg (String& r, String arg)
{
  int p = r.index_i ('%');
  if (p < 0)
	return ;

  r = r.left_str (p) + arg + r.right_str (r.length_i () - p - 1);
}

String
Lookup::substitute_args (String source, Array<String> args) const
{
  String str (source);
  for (int i = 0 ; i < args.size (); i++)
    substitute_arg (str, args[i]);
  return str;
}

String
Lookup::substitute_args (String source, Array<Scalar> args) const
{
  Array<String> sv;
  for (int i = 0 ; i < args.size (); i++)
    sv.push (args[i]);
  return substitute_args (source, sv);
}

Atom
Lookup::stem (Real y1, Real y2, String str) const
{
  if (y1 > y2)
    {
      Real t = y1;
      y1 = y2;
      y2 = t;
    }
  Atom s;

  s.dim_.x () = Interval (0,0);
  s.dim_.y () = Interval (y1,y2);

  Array<String> a;

  Real stem_width = paper_l_->get_var ("stemthickness");
  a.push (print_dimen (-stem_width /2));
  a.push (print_dimen (stem_width));
  a.push (print_dimen (y2));
  a.push (print_dimen (-y1));

  s.str_ = substitute_args (str, a);
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
Lookup::text (String style, String text) const
{
  Array<String> a;

  a.push (text);
  Atom s =  (*symtables_p_) ("style")->lookup (style);
  s.str_ = substitute_args (s.str_,a);
  s.font_ = font_;

  return s;
}

Atom
Lookup::time_signature (Array<Scalar> a) const
{
  Atom s ((*symtables_p_) ("param")->lookup ("time_signature"));
  s.str_ = substitute_args (s.str_, a);

  return s;
}

/*
  should be handled via Tex_ code and Lookup::bar ()
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

  
  int idx = int (rint ( (y- min_y)/step)) + 1;
  
  {
    Array<String> a;
    a.push (character_str (idx));
    brace.str_ = substitute_args (brace.str_,a);
    brace.dim_[Y_AXIS] = Interval (-y/2,y/2);
  }

  brace.font_ = font_;

  return brace;
}


