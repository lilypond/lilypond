/*
  lookup.cc -- implement simple Lookup methods.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>

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
#include "atom.hh"

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
Lookup::ledger_line (Interval xwid) const
{
  Molecule end (afm_find ("noteheads-ledgerending"));
  Interval ed = end.dim_[X_AXIS];
  xwid = Interval (xwid[LEFT] - ed[LEFT],
		   xwid[RIGHT] - ed[RIGHT]);
  Molecule mid = filledbox (Box (xwid, end.dim_[Y_AXIS]));
  Molecule l (mid);

  Molecule e2 = end;
  Molecule e1 = end;
  e1.translate_axis (xwid[RIGHT], X_AXIS);
  e2.translate_axis (xwid[LEFT], X_AXIS);

  l.add_molecule (e1);
  l.add_molecule (e2);
  return l;
}


Molecule
Lookup::accidental (int j, bool cautionary) const
{
  Molecule m(afm_find (String ("accidentals-") + to_str (j)));
  if (cautionary) 
    {
      Molecule open = afm_find (String ("accidentals-("));
      Molecule close = afm_find (String ("accidentals-)"));
      m.add_at_edge(X_AXIS, LEFT, Molecule(open), 0);
      m.add_at_edge(X_AXIS, RIGHT, Molecule(close), 0);
    }
  return m;
}



Molecule
Lookup::afm_find (String s, bool warn) const
{
  if (!afm_l_)      
    {
      Lookup * me =     (Lookup*)(this);
      me->afm_l_ = all_fonts_global_p->find_afm (font_name_);
      if (!me->afm_l_)
	{
	  warning (_f("Can't open `%s'\n", font_name_));
	  warning (_f("Search path %s\n", global_path.str ().ch_C()));
	  error (_f("Aborting"));
	}
    }
  Adobe_font_char_metric cm = afm_l_->find_char (s, warn);
  Molecule m;
  if (cm.code () < 0)
    return m;
    
  Atom at (gh_list (ly_symbol ("char"),
		    gh_int2scm (cm.code ()),
		    SCM_UNDEFINED));
  at.font_ = ly_symbol (font_name_.ch_C());
  m.dim_ = cm.dimensions();
  m.add_atom (&at);
  return m;
}

Molecule
Lookup::ball (int j) const
{
  if (j > 2)
    j = 2;

  return afm_find (String ("noteheads-") + to_str (j));
}

Molecule
Lookup::simple_bar (String type, Real h) const
{
  SCM thick = ly_symbol ("barthick_" + type);
  Real w = 0.1 PT;
  if (paper_l_->scope_p_->elem_b (thick))
    {
      w = paper_l_->get_realvar (thick);
    }

  return filledbox (Box (Interval(0,w), Interval(-h/2, h/2)));
}

  
Molecule
Lookup::bar (String str, Real h) const
{
  if (str == "[")
    return staff_bracket (h);
  else if (str == "{")
    return staff_brace (h);
  
  Real kern = paper_l_->get_var ("bar_kern");
  Real thinkern = paper_l_->get_var ("bar_thinkern");  
  Molecule thin = simple_bar ("thin", h);
  Molecule thick = simple_bar ("thick", h);
  Molecule colon = afm_find ("dots-repeatcolon");  

  Molecule m;
  
  if (str == "")
    {
      return fill (Box (Interval(0, 0), Interval (-h/2, h/2)));
    }
  else if (str == "|")
    {
      return thin;
    }
  else if (str == "|.")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);      
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
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
      m.add_at_edge (X_AXIS, RIGHT, thick, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);      
    }
  else if (str == ":|:")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);      
      m.add_at_edge (X_AXIS, RIGHT, thick, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);      
    }
  else if (str == ".|.")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern);
      m.add_at_edge (X_AXIS, RIGHT, thick, kern);      
    }
  else if (str == "||")
    {
      m.add_at_edge (X_AXIS, RIGHT, thin, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, thinkern);      
    }

  return m;
}

Molecule 
Lookup::beam (Real slope, Real width, Real thick) const
{
  Real height = slope * width; 
  Real min_y = (0 <? height) - thick/2;
  Real max_y = (0 >? height) + thick/2;

  
  Molecule m;
  Atom at
     (gh_list (ly_symbol ("beam"),
				gh_double2scm (width),
				gh_double2scm (slope),
				gh_double2scm (thick),
				SCM_UNDEFINED));

  m.dim_[X_AXIS] = Interval (0, width);
  m.dim_[Y_AXIS] = Interval (min_y, max_y);
  m.add_atom (&at);
  return m;
}

Molecule
Lookup::clef (String st) const
{
  return afm_find (String ("clefs-" + st));
}

SCM
offset2scm (Offset o)
{
  return gh_list (gh_double2scm (o[X_AXIS]), gh_double2scm(o[Y_AXIS]),
		  SCM_UNDEFINED);
}

Molecule
Lookup::dashed_slur (Array<Offset> controls, Real thick, Real dash) const
{
  assert (controls.size () == 8);
  Offset d = controls[3] - controls[0];
  
  Real dx = d[X_AXIS];
  Real dy = d[Y_AXIS];

  Molecule m;


  m.dim_[X_AXIS] = Interval (0, dx);
  m.dim_[Y_AXIS] = Interval (0 <? dy, 0 >? dy);

  SCM sc[4];
  for (int i=0; i<  4; i++)
    {
      sc[i] =  offset2scm (controls[i]);
    }

  Atom at
     (gh_list (ly_symbol ("dashed-slur"),
				gh_double2scm (thick), 
				gh_double2scm (dash),
				ly_quote_scm (array_to_list (sc, 4)),
				SCM_UNDEFINED));
  
  
  m.add_atom (&at);
  return m;
}

Molecule
Lookup::dots () const
{
  return afm_find (String ("dots-dot"));
}



Molecule
Lookup::fill (Box b) const
{
  Molecule m;
  m.dim_ = b;
  return m;
}

Molecule
Lookup::flag (int j, Direction d) const
{
  char c = (d == UP) ? 'u' : 'd';
  return  afm_find (String ("flags-") + to_str (c) + to_str (j));
}

Molecule
Lookup::rest (int j, bool o) const
{
  return afm_find (String ("rests-") + to_str (j) + (o ? "o" : ""));
}

Molecule
Lookup::rule_symbol (Real height, Real width) const
{
  Atom at  (gh_list (ly_symbol ("rulesym"),
					 gh_double2scm (height),
					 gh_double2scm (width),
					 SCM_UNDEFINED));

  Molecule m;
  m.dim_.x () = Interval (0, width);
  m.dim_.y () = Interval (0, height);
  
  m.add_atom (&at);

  return m;
}

Molecule
Lookup::script (String str) const
{
  return afm_find (String ("scripts-") + str);
}

Molecule
Lookup::special_time_signature (String s, int n, int d) const
{
  // First guess: s contains only the signature style
  String symbolname = "timesig-" + s + to_str (n) + "/" + to_str (d);
  
  Molecule m = afm_find (symbolname, false);
  if (!m.dim_[X_AXIS].empty_b ()) 
    return m;

  // Second guess: s contains the full signature name
  m = afm_find ("timesig-"+s, false);
  if (!m.dim_[X_AXIS].empty_b ()) 
    return m;

  // Resort to default layout with numbers
  return time_signature (n,d);
}

Molecule
Lookup::filledbox (Box b ) const
{
  Molecule m;
  
  Atom at  (gh_list (ly_symbol ("filledbox"),
					 gh_double2scm (-b[X_AXIS][LEFT]),
					 gh_double2scm (b[X_AXIS][RIGHT]),		       
					 gh_double2scm (-b[Y_AXIS][DOWN]),
					 gh_double2scm (b[Y_AXIS][UP]),		       
					 SCM_UNDEFINED));

  m.dim_ = b;
  m.add_atom (&at);
  return m;
}

Molecule
Lookup::stem (Real y1, Real y2) const
{
  if (y1 > y2)
    {
      Real t = y1;
      y1 = y2;
      y2 = t;
    }
  Real stem_width = paper_l_->get_var ("stemthickness");
  return filledbox (Box (Interval (-stem_width/2,stem_width/2),
			 Interval (y1, y2)));
}


static Dict_initialiser<char const*> cmr_init[] = {
  {"bold", "cmbx"},
  {"dynamic", "feta-din"},
  {"finger", "feta-nummer"},
  {"typewriter", "cmtt"},
  {"italic", "cmti"},
  {"roman", "cmr"},
  {"large", "cmbx"},
  {"Large", "cmbx"},
  {"mark", "feta-nummer"},
  {"number", "feta-nummer"},
  {"volta", "feta-nummer"},
  {0,0}
};

static Dictionary<char const *> cmr_dict (cmr_init);

Molecule
Lookup::text (String style, String text) const
{
  Molecule m;
  

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
  Interval ydims (0,0);

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
	  ydims.unite (c->dimensions()[Y_AXIS]);
	}
    }

  DOUT << "\n" << to_str (w) << "\n";
  m.dim_.x () = Interval (0, w);
  m.dim_.y () = ydims;
  Atom at  (gh_list (ly_symbol ("text"),
		     gh_str02scm (text.ch_C()),
		     SCM_UNDEFINED));
  at.font_ = ly_symbol (style);
  
  m.add_atom (&at);
  return m;
}
  

/*
 */
Molecule
Lookup::time_signature (int num, int den) const
{
  String sty = "number";
  Molecule n (text (sty, to_str (num)));
  Molecule d (text (sty, to_str (den)));
  n.do_center (X_AXIS);
  d.do_center (X_AXIS);
  Molecule m;
  if (den)
    {
      m.add_at_edge (Y_AXIS, UP, n, 0.0);
      m.add_at_edge (Y_AXIS, DOWN, d, 0.0);
    }
  else
    {
      m = n;
      m.do_center (Y_AXIS);
    }
  return m;
}

Molecule
Lookup::staff_brace (Real y) const
{
  Molecule m;
  
  Atom at  (gh_list (ly_symbol ("pianobrace"),
		       gh_double2scm (y),
		       SCM_UNDEFINED
		       ));
  
  m.dim_[Y_AXIS] = Interval (-y/2,y/2);
  m.dim_[X_AXIS] = Interval (0,0);
  m.add_atom (&at);
  return m;
}

Molecule
Lookup::hairpin (Real width, bool decresc, bool continued) const
{
  Molecule m;   
  Real height = paper_l_->staffheight_f () / 6;

  String hairpin = String (decresc ? "de" : "") + "crescendo";
  Atom at  (gh_list (ly_symbol (hairpin),
		       gh_double2scm (width),
		       gh_double2scm (height),
		       gh_double2scm (continued ? height/2 : 0.0),
		       SCM_UNDEFINED));
  m.dim_.x () = Interval (0, width);
  m.dim_.y () = Interval (-2*height, 2*height);

  m.add_atom (&at);
  return m;
}

Molecule
Lookup::plet (Real dy , Real dx, Direction dir) const
{
  Molecule m;
  SCM thick = ly_symbol ("tuplet_thick");
  Real t = 0.1 PT;
  if (paper_l_->scope_p_->elem_b (thick))
    {
      t = paper_l_->get_realvar (thick);
    }
  
  Atom at  (gh_list(ly_symbol ("tuplet"),
		      gh_double2scm (dx),
		      gh_double2scm (dy),
		      gh_double2scm (t),
		      gh_int2scm (dir),
		      SCM_UNDEFINED));
m.add_atom (&at);

  return m;
}

/*
  Make a smooth curve along the points 
 */
Molecule
Lookup::slur (Array<Offset> controls) const
{
  Offset  delta_off = controls[3]- controls[0];
  Molecule m; 

  SCM scontrols [8];
  int indices[] = {5,6,7,4,1,2,3,0};

  for (int i= 0; i < 8; i++)
    scontrols[i] = offset2scm (controls[indices[i]]);


  Atom at  (gh_list (ly_symbol ("bezier-sandwich"),
		      ly_quote_scm (array_to_list (scontrols, 8)),
		      SCM_UNDEFINED));

  m.dim_[X_AXIS] = Interval (0, delta_off[X_AXIS]);
  m.dim_[Y_AXIS] = Interval (0 <? delta_off[Y_AXIS], 0 >? delta_off[Y_AXIS]);
  m.add_atom (&at);
  return m;
}

Molecule
Lookup::staff_bracket (Real y) const
{
  Molecule m; 
  Atom at  ( gh_list (ly_symbol ("bracket"),
			gh_double2scm (y),
			SCM_UNDEFINED));
  m.add_atom (&at);				 
  m.dim_[Y_AXIS] = Interval (-y/2,y/2);
  m.dim_[X_AXIS] = Interval (0,4 PT);

  m.translate_axis (- 4. / 3. * m.dim_[X_AXIS].length (), X_AXIS);
  return m;
}

Molecule
Lookup::volta (Real w, bool last_b) const
{
  Molecule m; 
  SCM thick = ly_symbol ("volta_thick");
  Real t = 0.1 PT;
  if (paper_l_->scope_p_->elem_b (thick))
    {
      t = paper_l_->get_realvar (thick);
    }
  Atom at  (gh_list (ly_symbol ("volta"),
		       gh_double2scm (w),
		       gh_double2scm (t),
		       gh_int2scm (last_b),
		       SCM_UNDEFINED));

  Real interline_f = paper_l_->interline_f ();

  m.dim_[Y_AXIS] = Interval (-interline_f, interline_f);
  m.dim_[X_AXIS] = Interval (0, w);

  m.add_atom (&at);
  return m;
}


Molecule
Lookup::special_ball (int j, String kind_of_ball) const
{
  if (j > 2)
    j = 2;

  return afm_find (String ("noteheads-") + kind_of_ball);
}

