/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  Jan Nieuwenhuizen <janneke@gnu.org>

  TODO
      Read spacing info from AFMs
      Glissando
*/

#include <ctype.h>
#include "lookup.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "symtable.hh"
#include "scalar.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "file-path.hh"
#include "main.hh"
#include "lily-guile.hh"

SCM
array_to_list (SCM *a , int l)
{
  SCM list = SCM_EOL;
  for (int i= l; i--;  )
    {
      list =  gh_cons (a[i], list);
    }
  return list;
}


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

Molecule
Lookup::accidental (int j, bool cautionary) const
{
  Molecule m(afm_find (String ("accidentals") + String ("-") + to_str (j)));
  if (cautionary) 
    {
      m.add_at_edge(X_AXIS, LEFT, 
                    Molecule(afm_find (String ("accidentals") + String ("-("))))
;
      m.add_at_edge(X_AXIS, RIGHT, 
                    Molecule(afm_find (String ("accidentals") + String ("-)"))))
;
    }
  return m;
}

void
Lookup::add (String s, Symtable*p)
{
  symtables_p_->add (s, p);
}

Atom
Lookup::afm_find (String s, bool warn) const
{
  if (!afm_p_)
    {
      *mlog << "[" << font_path_;
      ( (Lookup*)this)->afm_p_ = new Adobe_font_metric (read_afm (font_path_));
      *mlog << "]" << flush ;
      DOUT << this->afm_p_->str ();
    }
  Adobe_font_char_metric m = afm_p_->find_char (s, warn);

  Atom a;
  if (m.code () < 0)
    return a;
  
  a.dim_ = m.B_;
  a.dim_[X_AXIS] *= 1 / 1000.0;
  a.dim_[Y_AXIS] *= 1 / 1000.0;

  
  a.lambda_ = gh_list (ly_symbol ("char"),
		       gh_int2scm (m.code ()),
		       SCM_UNDEFINED);
  a.str_ = "afm_find: " + s;
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

  Atom a = (*symtables_p_) ("bars")->lookup (str);
  
  
  a.lambda_ = gh_list (ly_symbol (a.str_.ch_C()),
		       gh_double2scm (h),
		       SCM_UNDEFINED);


  a.dim_.y () = Interval (-h/2, h/2);
  a.font_ = font_;
  return a;
}

Atom 
Lookup::beam (Real slope, Real width, Real thick) const
{
  Real height = slope * width; 
  Real min_y = (0 <? height) - thick/2;
  Real max_y = (0 >? height) + thick/2;

  Atom a;
  a.lambda_ =   gh_list (ly_symbol ("beam"),
	   gh_double2scm (width),
	   gh_double2scm (slope),
	   gh_double2scm (thick),
	   SCM_UNDEFINED);

  a.dim_[X_AXIS] = Interval (0, width);
  a.dim_[Y_AXIS] = Interval (min_y, max_y);
  return a;
}

Atom
Lookup::clef (String st) const
{
  return afm_find (String ("clefs") + String ("-") + st);
}

SCM
offset2scm (Offset o)
{
  return gh_list (gh_double2scm (o[X_AXIS]), gh_double2scm(o[Y_AXIS]),
		  SCM_UNDEFINED);
}

Atom
Lookup::dashed_slur (Array<Offset> controls, Real thick, Real dash) const
{
  assert (controls.size () == 8);
  Offset d = controls[3] - controls[0];
  
  Real dx = d[X_AXIS];
  Real dy = d[Y_AXIS];

  Atom a;
  a.font_ = font_;
  a.dim_[X_AXIS] = Interval (0, dx);
  a.dim_[Y_AXIS] = Interval (0 <? dy,  0 >? dy);

  SCM sc[4];
  for (int i=0; i<  4; i++)
    {
      sc[i] =  offset2scm (controls[i]);
    }

  a.lambda_ = 
    gh_list (ly_symbol ("dashed-slur"),
	     gh_double2scm (thick), 
	     gh_double2scm (dash),
	     ly_quote_scm (array_to_list (sc, 4)),
	     SCM_UNDEFINED);

  a.str_ = "dashed_slur";
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
Lookup::extender (Real width) const
{
  Atom a = (*symtables_p_) ("param")->lookup ("extender");
  a.lambda_ = gh_list (ly_symbol (a.str_),
		       gh_double2scm (width),
		       SCM_UNDEFINED);
  a.str_ = "extender";
  a.font_ = font_;
  return a;
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
  Atom a = afm_find (String ("flags") + String ("-") + to_str (c) + to_str (j));
  return a;
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
  Atom a;
  a.lambda_ = gh_list (ly_symbol ("rulesym"),
		       gh_double2scm (height),
		       gh_double2scm (width),
		       SCM_UNDEFINED);
  a.dim_.x () = Interval (0, width);
  a.dim_.y () = Interval (0, height);
  return a;
}

Atom
Lookup::script (String str) const
{
  return afm_find (String ("scripts") + String ("-") + str);
}

Atom
Lookup::special_time_signature (String s, Array<int> arr) const
{
  // First guess: s contains only the signature style
  assert (arr.size () >1);
  String symbolname = "timesig-" + s + to_str (arr[0]) + "/" + to_str (arr[1]);
  
  Atom a = afm_find (symbolname, false);
  if (!a.empty ()) 
    return a;

  // Second guess: s contains the full signature name
  a = afm_find ("timesig-"+s, false);
  if (!a.empty ()) 
    return a;

  // Resort to default layout with numbers
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
  Atom a;

  a.dim_.x () = Interval (0,0);
  a.dim_.y () = Interval (y1,y2);

  Real stem_width = paper_l_->get_var ("stemthickness");

  a.lambda_ = gh_list (ly_symbol ("stem"),
		       gh_double2scm(-stem_width /2),
		       gh_double2scm(stem_width),
		       gh_double2scm(y2),
		       gh_double2scm(-y1),
		       SCM_UNDEFINED);

  a.font_ = font_;
  return a;
}

Atom
Lookup::streepje (int type) const
{
  if (type > 2)
    type = 2;

  return  afm_find ("balls" + String ("-") +to_str (type) + "l");
}

Dictionary<String> cmr_dict;
Dictionary<Adobe_font_metric*> afm_p_dict;

Atom
Lookup::text (String style, String text) const
{
  Atom a =  (*symtables_p_) ("style")->lookup (style);

  a.lambda_ = gh_list(ly_symbol (a.str_),
		      gh_str02scm (text.ch_C()),
		      SCM_UNDEFINED);
  
  Real font_w = a.dim_.x ().length ();
  Real font_h = a.dim_.y ().length ();

  if (!cmr_dict.elem_b ("roman"))
    {
      //brrrr
      cmr_dict.elem ("bold") = "cmbx";
      cmr_dict.elem ("dynamic") = "feta-din";
      cmr_dict.elem ("finger") = "feta-nummer";
      cmr_dict.elem ("italic") = "cmti";
      cmr_dict.elem ("roman") = "cmr";
    }

  if (!afm_p_dict.elem_b (style))
    {
      Adobe_font_metric* afm_p = 0;
      String cmr_str = cmr_dict.elem (style) + to_str ((int) font_h) + ".afm";
      String font_path = global_path.find (cmr_str);
      if (!font_path.length_i ())
        {
	  warning (_f("can't open file: `%s'", cmr_str.ch_C ()));
	  warning (_f("guessing dimensions for font style: `%s'", style.ch_C ()));
	}
      else
        {
	  *mlog << "[" << font_path;
	  afm_p = new Adobe_font_metric (read_afm (font_path));
	  DOUT << afm_p->str ();
	  *mlog << "]" << flush ;
	}
      afm_p_dict.elem (style) = afm_p;
    }
  Real w = 0;
  Adobe_font_metric* afm_p = afm_p_dict.elem (style);
  DOUT << "\nChars: ";
  for (int i = 0; i < text.length_i (); i++) 
    {
      if (text[i]=='\\')
	for (i++; (i < text.length_i ()) && isalpha(text[i]); i++)
	  ;
      else
	{
	  int c = text[i];
	  if (afm_p && ((c >= 0) && (c < afm_p->char_metrics_.size ())))
	    {
	      Adobe_font_char_metric m = afm_p->char_metrics_[c];
	      w += m.B_.x ().length ();
	      DOUT << to_str (m.B_.x ().length ()) << " ";
	    }
	  else
	      w += font_w;
	}
    }
  DOUT << "\n" << to_str (w) << "\n";
  a.dim_.x () = Interval (0, w);
  a.font_ = font_;
  return a;
}
  

Atom
Lookup::time_signature (Array<int> a) const
{
  Atom s ((*symtables_p_) ("param")->lookup ("time_signature"));
  s.lambda_ = gh_list (ly_symbol (s.str_),
		       gh_int2scm (a[0]),
		       gh_int2scm (a[1]),
		       SCM_UNDEFINED);
  return s;
}

Atom
Lookup::vbrace (Real &y) const
{
  Atom a;
  a.lambda_ = gh_list (ly_symbol ("pianobrace"),
		       gh_double2scm (y),
		       SCM_UNDEFINED
		       );
  a.dim_[Y_AXIS] = Interval (-y/2,y/2);
  a.font_ = font_;
  return a;
}

Atom
Lookup::hairpin (Real width, bool decresc, bool continued) const
{
  Atom a;  
  Real height = paper_l_->staffheight_f () / 6;

  String hairpin = String (decresc ? "de" : "") + "crescendo";
  a.lambda_ = gh_list (ly_symbol (hairpin),
		       gh_double2scm (width),
		       gh_double2scm (height),
		       gh_double2scm (continued ? height/2 : 0.0),
		       SCM_UNDEFINED);
  a.dim_.x () = Interval (0, width);
  a.dim_.y () = Interval (-2*height, 2*height);
  a.font_ = font_;
  return a;
}

Atom
Lookup::plet (Real dy , Real dx, Direction dir) const
{
  Atom a;
  a.lambda_ = gh_list(ly_symbol ("tuplet"),
		      gh_double2scm (dx),
		      gh_double2scm (dy),
		      gh_int2scm (dir), SCM_UNDEFINED);
  return a;
}


Atom
Lookup::slur (Array<Offset> controls) const
{
  assert (controls.size () == 8);
  Real dx = controls[3].x () - controls[0].x ();
  Real dy = controls[3].y () - controls[0].y ();
  Atom a;

  SCM scontrols [8];
  int indices[] = {5,6,7,4,1,2,3,0};

  for (int i= 0; i < 8; i++)
    scontrols[i] = offset2scm (controls[indices[i]]);


  a.lambda_ =gh_list (ly_symbol ("slur"),
		      ly_quote_scm (array_to_list (scontrols, 8)),
		      SCM_UNDEFINED);

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

  a.lambda_ =  gh_list (ly_symbol ("bracket"),
			gh_double2scm (y),
			SCM_UNDEFINED);
  a.str_ = "vbracket";
  a.dim_[Y_AXIS] = Interval (-y/2,y/2);
  a.dim_[X_AXIS] = Interval (0,4 PT);
  return a;
}

Atom
Lookup::volta (Real w, bool last_b) const
{
  Atom a;
  a.lambda_ = gh_list (ly_symbol ("volta"),
		       gh_double2scm (w),
		       gh_int2scm (last_b),
		       SCM_UNDEFINED);
  a.str_ = "volta";
  Real interline_f = paper_l_->interline_f ();
//  a.dim_[Y_AXIS] = Interval (0, 2 * interline_f);
  a.dim_[Y_AXIS] = Interval (-interline_f, interline_f);
  a.dim_[X_AXIS] = Interval (0, w);
  return a;
}

