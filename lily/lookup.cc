/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  Jan Nieuwenhuizen <janneke@gnu.org>

  TODO
      Glissando
*/

#include <ctype.h>
#include "lookup.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "scalar.hh"
#include "paper-def.hh"
#include "string-convert.hh"
#include "file-path.hh"
#include "main.hh"
#include "lily-guile.hh"
#include "all-fonts.hh"
#include "afm.hh"
#include "scope.hh"
#include "molecule.hh"

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
  afm_l_ = 0;  
}

Lookup::Lookup (Lookup const& s)
{
  font_name_ = s.font_name_;
  paper_l_ = 0;
  afm_l_ = 0;  
}



Molecule
Lookup::accidental (int j, bool cautionary) const
{
  Molecule m(afm_find (String ("accidentals") + String ("-") + to_str (j)));
  if (cautionary) 
    {
      Atom open = afm_find (String ("accidentals") + String ("-("));
      Atom close = afm_find (String ("accidentals") + String ("-)"));
      m.add_at_edge(X_AXIS, LEFT, Molecule(open), 0);
      m.add_at_edge(X_AXIS, RIGHT, Molecule(close), 0);
    }
  return m;
}



Atom
Lookup::afm_find (String s, bool warn) const
{
  if (!afm_l_)      
    {
      Lookup * me =     ((Lookup*)this);
      me->afm_l_ = all_fonts_global_p->find_afm (font_name_);
      if (!me->afm_l_)
	{
	  warning (_f("Can't open `%s'\n", font_name_));
	  warning (_f("Search path %s\n", global_path.str ().ch_C()));
	}
    }
  Adobe_font_char_metric m = afm_l_->find_char (s, warn);

  Atom a;
  if (m.code () < 0)
    return a;
    
  a.dim_ = m.dimensions();
  
  a.lambda_ = gh_list (ly_symbol ("char"),
		       gh_int2scm (m.code ()),
		       SCM_UNDEFINED);
  a.font_ = font_name_;
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
Lookup::simple_bar (String type, Real h) const
{
  SCM thick = ly_symbol ("barthick_" + type);
  Real w = 0.1 PT;
  if (paper_l_->scope_p_->elem_b (thick))
    {
      w = paper_l_->get_realvar (thick);
    }
  
  Atom a;
  a.lambda_ = gh_list (ly_symbol ("filledbox"),
		       gh_double2scm (0),
		       gh_double2scm (w),		       
		       gh_double2scm (h/2),
		       gh_double2scm (h/2),		       
		       SCM_UNDEFINED);

  a.dim_[X_AXIS] = Interval(0,w);
  a.dim_[Y_AXIS] = Interval (-h/2, h/2);
  return a;
}

  
Molecule
Lookup::bar (String str, Real h) const
{
  Real kern = paper_l_->get_var ("bar_kern");
  Real thinkern = paper_l_->get_var ("bar_thinkern");  
  Atom thin = simple_bar ("thin", h);
  Atom thick = simple_bar ("thick", h);
  Atom colon = afm_find ("dots-repeatcolon");  

  Molecule m;

  if (str == "")
    {
      return fill (Box (Interval(0,0),Interval (-h/2, h/2)));
    }
  else if (str == "|")
    {
      return thin;
    }
  else if (str == "|.")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);      
      m.add_at_edge (X_AXIS, LEFT, thin,kern);
    }
  else if (str == ".|")
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
    }
  else if (str == ":|")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);      
    }
  else if (str == "|:")
    {
      m.add_at_edge (X_AXIS, RIGHT, thick,0);
      m.add_at_edge (X_AXIS, RIGHT, thin,kern);
      m.add_at_edge (X_AXIS, RIGHT, colon,kern);      
    }
  else if (str == ":|:")
    {
      m.add_at_edge (X_AXIS, LEFT, thick,thinkern);
      m.add_at_edge (X_AXIS, LEFT, colon,kern);      
      m.add_at_edge (X_AXIS, RIGHT, thick,kern);
      m.add_at_edge (X_AXIS, RIGHT, colon,kern);      
    }
  else if (str == "||")
    {
      m.add_at_edge (X_AXIS, RIGHT, thin,0);
      m.add_at_edge (X_AXIS, RIGHT, thin,thinkern);      
    }

  else if (str == ".|.")
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0);
      m.add_at_edge (X_AXIS, RIGHT, thick, kern);      
    }

  return m;
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
  a.font_ = font_name_;
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

  return a;
}

Atom
Lookup::dots () const
{
  return afm_find (String ("dots") + String ("-") + String ("dot"));
}


Atom
Lookup::extender (Real width) const
{
  Atom a;
  a.lambda_ = gh_list (ly_symbol ("extender"),
		       gh_double2scm (width),
		       SCM_UNDEFINED);

  a.dim_[X_AXIS] = Interval (0, width);
  a.dim_[Y_AXIS] = Interval (0,0);
  
  a.font_ = font_name_;
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

  a.lambda_ = gh_list (ly_symbol ("filledbox"),
		       gh_double2scm(stem_width /2),
		       gh_double2scm(stem_width/2),
		       gh_double2scm(y2),
		       gh_double2scm(-y1),
		       SCM_UNDEFINED);

  a.font_ = font_name_;
  return a;
}

Atom
Lookup::streepje (int type) const
{
  if (type > 2)
    type = 2;

  return  afm_find ("balls" + String ("-") +to_str (type) + "l");
}

static Dict_initialiser<char const*> cmr_init[] = {
  {"bold", "cmbx"},
  {"dynamic", "feta-din"},
  {"finger", "feta-nummer"},
  {"italic", "cmti"},
  {"roman", "cmr"},
  {"large", "cmbx"},
  {"Large", "cmbx"},
  {"mark", "feta-nummer"},
  {"nummer", "feta-nummer"},
  {0,0}
};

static Dictionary<char const *> cmr_dict (cmr_init);

Atom
Lookup::text (String style, String text) const
{
  Atom a;
  a.lambda_ = gh_list(ly_symbol ("set" + style),
		      gh_str02scm (text.ch_C()),
		      SCM_UNDEFINED);

  Real font_h = paper_l_->get_var ("font_normal");
  if (paper_l_->scope_p_->elem_b ("font_" + style))
    {
      font_h = paper_l_->get_var ("font_" + style);
    }
  
  if (cmr_dict.elem_b (style))
    {
      style = String (cmr_dict [style]) + to_str  ((int)font_h); // ugh
    }

  Real w = 0;
  Real h = 0;
  Real d = 0;

  Font_metric* afm_l = all_fonts_global_p->find_font (style);
  DOUT << "\nChars: ";
  
  for (int i = 0; i < text.length_i (); i++) 
    {
      if (text[i]=='\\')
	for (i++; (i < text.length_i ()) && isalpha(text[i]); i++)
	  ;
      else
	{
	  Character_metric *c = afm_l->get_char (text[i],false);
	  w += c->dimensions()[X_AXIS].length ();
 	  h = h >? c->dimensions()[Y_AXIS].max ();
	  d = d <? c->dimensions()[Y_AXIS].min ();
	}
    }

  DOUT << "\n" << to_str (w) << "\n";
  a.dim_.x () = Interval (0, w);
  a.dim_.y () = Interval (d, h);
  a.font_ = font_name_;
  return a;
}
  

/*
  TODO: should return a molecule with 2 stacked nums.
 */
Atom
Lookup::time_signature (Array<int> a) const
{
  Atom s;
  s.lambda_ = gh_list (ly_symbol ("generalmeter"),
		       gh_int2scm (a[0]),
		       gh_int2scm (a[1]),
		       SCM_UNDEFINED);

  Real r = paper_l_->interline_f () ;
  s.dim_[Y_AXIS] =  Interval (-2*r, 2*r);
  s.dim_[X_AXIS] = Interval (0, 2*r);
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
  a.dim_[X_AXIS] = Interval (0,0);
  a.font_ = font_name_;
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
  a.font_ = font_name_;
  return a;
}

Atom
Lookup::plet (Real dy , Real dx, Direction dir) const
{
  Atom a;
  SCM thick = ly_symbol ("tuplet_thick");
  Real t = 0.1 PT;
  if (paper_l_->scope_p_->elem_b (thick))
    {
      t = paper_l_->get_realvar (thick);
    }
  a.lambda_ = gh_list(ly_symbol ("tuplet"),
		      gh_double2scm (dx),
		      gh_double2scm (dy),
		      gh_double2scm (t),
		      gh_int2scm (dir),
		      SCM_UNDEFINED);
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
  a.font_ = font_name_;
  return a;
}

Atom
Lookup::vbracket (Real &y) const
{
  Atom a;
  a.lambda_ =  gh_list (ly_symbol ("bracket"),
			gh_double2scm (y),
			SCM_UNDEFINED);
  a.dim_[Y_AXIS] = Interval (-y/2,y/2);
  a.dim_[X_AXIS] = Interval (0,4 PT);
  return a;
}

Atom
Lookup::volta (Real w, bool last_b) const
{
  Atom a;
  SCM thick = ly_symbol ("volta_thick");
  Real t = 0.1 PT;
  if (paper_l_->scope_p_->elem_b (thick))
    {
      t = paper_l_->get_realvar (thick);
    }
  a.lambda_ = gh_list (ly_symbol ("volta"),
		       gh_double2scm (w),
		       gh_double2scm (t),
		       gh_int2scm (last_b),
		       SCM_UNDEFINED);

  Real interline_f = paper_l_->interline_f ();

  a.dim_[Y_AXIS] = Interval (-interline_f, interline_f);
  a.dim_[X_AXIS] = Interval (0, w);
  return a;
}


Atom
Lookup::special_ball (int j, String kind_of_ball) const
{
  if (j > 2)
    j = 2;

  return afm_find (String ("balls") + String ("-") + kind_of_ball);
}
