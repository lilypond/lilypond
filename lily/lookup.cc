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
#include "lily-guile.hh"

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
Lookup::afm_find (String s) const
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
  Array<Real> arr;
  arr.push (m.code ());
  a.lambda_ = lambda_scm ("char", arr);
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
  Array<Real> arr;
  arr.push (h);
  Atom a = (*symtables_p_) ("bars")->lookup (str);
  a.lambda_ = lambda_scm (a.str_, arr);
  a.dim_.y () = Interval (-h/2, h/2);
  a.font_ = font_;
  return a;
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

Atom
Lookup::clef (String st) const
{
  return afm_find (String ("clefs") + String ("-") + st);
}

Atom
Lookup::dashed_slur (Array<Offset> controls, Real thick, Real dash) const
{
  assert (controls.size () == 8);

  Real dx = controls[3].x () - controls[0].x ();
  Real dy = controls[3].y () - controls[0].y ();

  Atom a;
  a.font_ = font_;
  a.dim_[X_AXIS] = Interval (0, dx);
  a.dim_[Y_AXIS] = Interval (0 <? dy,  0 >? dy);

  // (lambda (o) (dashed-slur o '((0.1 0.2) (1.1 1.2) (2.1 2.2) (3.1 3.2))))
  a.lambda_ = 
    ly_append (ly_lambda_o (), 
    ly_list1 (ly_append (ly_func_o ("dashed-slur"),
    gh_cons (gh_double2scm (thick), gh_cons (gh_double2scm (dash),
    ly_list1 (ly_list2 (ly_quote (),
    gh_cons (ly_list2 (gh_double2scm (controls[1].x ()), gh_double2scm (controls[1].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[2].x ()), gh_double2scm (controls[2].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[3].x ()), gh_double2scm (controls[3].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[0].x ()), gh_double2scm (controls[0].y ())),
    SCM_EOL)))))))))));

  return a;
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
  Array<Real> args;
  args.push (height);
  args.push (width);
  bs.lambda_ = lambda_scm (bs.str_, args);
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
#if 0
  String symbolname = "timesig-"+s+"%/%";
  Atom a (afm_find (lambda_scm (symbolname, arr)));
  if (!a.empty ()) 
    return a;
  // Try if the full name was given
  a = afm_find ("timesig-"+s);
  if (!a.empty ()) 
    return a;
  // Resort to default layout with numbers
#endif
  return time_signature (arr);
}

Atom
Lookup::stem (Real y1, Real y2) const
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

  Array<Real> a;

  Real stem_width = paper_l_->get_var ("stemthickness");
  a.push (-stem_width /2);
  a.push (stem_width);
  a.push (y2);
  a.push (-y1);

  s.lambda_ = lambda_scm ("stem", a);
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
  Array<Scalar> a;

  a.push (text);
  Atom s =  (*symtables_p_) ("style")->lookup (style);
  s.lambda_ = lambda_scm (s.str_, a);
  s.font_ = font_;

  return s;
}

Atom
Lookup::time_signature (Array<Scalar> a) const
{
  Atom s ((*symtables_p_) ("param")->lookup ("time_signature"));
  s.lambda_ = lambda_scm (s.str_, a);

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
  
  Array<Real> a;
  a.push (idx);
  brace.lambda_ = lambda_scm (brace.str_, a);
  brace.dim_[Y_AXIS] = Interval (-y/2,y/2);

  brace.font_ = font_;

  return brace;
}

Atom
Lookup::hairpin (Real width, bool decresc, bool continued) const
{
  Atom a;  
  Real height = paper_l_->staffheight_f () / 6;
  String ps;
  ps += to_str (width) + " " 
	+ to_str (height) + " " 
    + to_str (continued ? height/2 : 0) + 
    + " draw_"  + String (decresc ? "de" : "") + "cresc\n";
  a.str_ = ps;


  a.dim_.x () = Interval (0, width);
  a.dim_.y () = Interval (-2*height, 2*height);
  a.font_ = font_;
  return a;
}

Atom
Lookup::plet (Real dy , Real dx, Direction dir) const
{
  String ps;
  
  
  ps += String_convert::double_str (dx) + " " 
    + String_convert::double_str (dy) + " "
    + String_convert::int_str ( (int)dir) +
    " draw_plet ";

  Atom s;
  s.str_ = ps;
  return s;
}

Atom
Lookup::ps_beam (Real slope, Real width, Real thick) const
{
  String ps;
  ps += to_str (width) + " "+ to_str (slope) + " " + to_str (thick)
    + " draw_beam ";

  Atom s;
  s.str_ = ps;
  return s;
}

Atom
Lookup::slur (Array<Offset> controls) const
{
  assert (controls.size () == 8);

  String ps;
  
  Real dx = controls[3].x () - controls[0].x ();
  Real dy = controls[3].y () - controls[0].y ();
  Atom a;
 
  // (lambda (o) (slur o '((0.1 0.2) (1.1 1.2) (2.1 2.2) (3.1 3.2) .. )))
  a.lambda_ = 
    ly_append (ly_lambda_o (), 
    ly_list1 (ly_append (ly_func_o ("slur"),
    ly_list1 (ly_list2 (ly_quote (),
    gh_cons (ly_list2 (gh_double2scm (controls[5].x ()), gh_double2scm (controls[5].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[6].x ()), gh_double2scm (controls[6].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[7].x ()), gh_double2scm (controls[7].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[4].x ()), gh_double2scm (controls[4].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[1].x ()), gh_double2scm (controls[1].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[2].x ()), gh_double2scm (controls[2].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[3].x ()), gh_double2scm (controls[3].y ())),
    gh_cons (ly_list2 (gh_double2scm (controls[0].x ()), gh_double2scm (controls[0].y ())),
    SCM_EOL)))))))))))));

  a.dim_[X_AXIS] = Interval (0, dx);
  a.dim_[Y_AXIS] = Interval (0 <? dy,  0 >? dy);
  a.font_ = font_;
  return a;
}

Atom
Lookup::vbracket (Real &y) const
{
  Atom a;
  Real min_y = paper_l_->staffheight_f ();
  if (y < min_y)
    {
      warning (_ ("bracket")
	+ " " + _ ("too small") +  " (" + print_dimen (y) + ")");
//      y = min_y;
    }
  Array<Real> arr;
  arr.push (y);
  a.lambda_ = lambda_scm ("bracket", arr);
  a.dim_[Y_AXIS] = Interval (-y/2,y/2);
  a.dim_[X_AXIS] = Interval (0,4 PT);
  return a;
}


