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
#include "lily-guile.hh"


Lookup::Lookup ()
{
  afm_l_ = 0;  
}

Lookup::Lookup (Lookup const& s)
{
  font_name_ = s.font_name_;
  afm_l_ = 0;  
}


/*
  build a ledger line for small pieces.
 */
Molecule
Lookup::ledger_line (Interval xwid) const
{
  Drul_array<Molecule> endings;
  endings[LEFT] = afm_find ("noteheads-ledgerending");
  Molecule * e = &endings[LEFT];
  endings[RIGHT] = *e;
  
  Real thick = e->dim_[Y_AXIS].length();
  Real len = e->dim_[X_AXIS].length () - thick;

  Molecule total;
  Direction d = LEFT;
  do {
    endings[d].translate_axis (xwid[d] - endings[d].dim_[X_AXIS][d], X_AXIS);
    total.add_molecule (endings[d]);    
  } while ((flip(&d)) != LEFT);

  Real xpos = xwid [LEFT] + len;

  while (xpos + len + thick /2 <= xwid[RIGHT])
    {
      e->translate_axis (len, X_AXIS);
      total.add_molecule (*e);
      xpos += len;
    }

  return total;
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
    {
      /*
	don't want people relying on this kind of dimension. 
       */
      m.set_empty (false);
      return m;
    }
  
  Atom at (gh_list (char_scm_sym,
		    gh_int2scm (cm.code ()),
		    SCM_UNDEFINED));
  at.font_ = ly_symbol (font_name_.ch_C());
  m.dim_ = cm.dimensions();
  m.add_atom (&at);
  return m;
}

Molecule
Lookup::notehead (int j, String type) const
{
  if (j > 2)
    j = 2;

  return afm_find (String ("noteheads-") + to_str (j) + type);
}

Molecule
Lookup::simple_bar (String type, Real h, Paper_def* paper_l) const
{
  SCM thick = ly_symbol ("barthick_" + type);
  Real w = 0.0;
  
  if (paper_l->scope_p_->elem_b (thick))
    {
      w = paper_l->get_realvar (thick);
    }
  else
    {
      programming_error ("No bar thickness set ! ");
      w = 1 PT;
    }
  return filledbox (Box (Interval(0,w), Interval(-h/2, h/2)));
}

  
Molecule
Lookup::bar (String str, Real h, Paper_def *paper_l) const
{
  if (str == "bracket")
    return staff_bracket (h);
  else if (str == "brace")
    return staff_brace (h);
  
  Real kern = paper_l->get_var ("bar_kern");
  Real thinkern = paper_l->get_var ("bar_thinkern");
  
  Molecule thin = simple_bar ("thin", h, paper_l);
  Molecule thick = simple_bar ("thick", h, paper_l);
  Molecule colon = afm_find ("dots-repeatcolon", paper_l);  

  Molecule m;
  
  if (str == "")
    {
      return fill (Box (Interval(0, 0), Interval (-h/2, h/2)));
    }
  if (str == "scorepostbreak")
    {
      return simple_bar ("score", h, paper_l);
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
     (gh_list (beam_scm_sym,
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
Lookup::rest (int j, bool o, String style) const
{
  return afm_find (String ("rests-") + to_str (j) + (o ? "o" : "") + style);
}


Molecule
Lookup::special_time_signature (String s, int n, int d, Paper_def*pap) const
{
  // First guess: s contains only the signature style
  String symbolname = "timesig-" + s + to_str (n) + "/" + to_str (d);
  
  Molecule m = afm_find (symbolname, false);
  if (!m.empty_b()) 
    return m;

  // Second guess: s contains the full signature name
  m = afm_find ("timesig-"+s, false);
  if (!m.empty_b ()) 
    return m;

  // Resort to default layout with numbers
  return time_signature (n,d,pap);
}

Molecule
Lookup::filledbox (Box b ) const
{
  Molecule m;
  
  Atom at  (gh_list (filledbox_scm_sym,
		     gh_double2scm (-b[X_AXIS][LEFT]),
		     gh_double2scm (b[X_AXIS][RIGHT]),		       
		     gh_double2scm (-b[Y_AXIS][DOWN]),
		     gh_double2scm (b[Y_AXIS][UP]),		       
		     SCM_UNDEFINED));

  m.dim_ = b;
  m.add_atom (&at);
  return m;
}



/**
   Magnification steps.  These are powers of 1.2. The numbers are
 taken from Knuth's plain.tex: */
static Real mag_steps[] = {1, 1, 1.200, 1.440, 1.7280,  2.074, 2.488};

/**
   TODO: THIS IS UGLY.  Since the user has direct access to TeX
   strings, we try some halfbaked attempt to detect TeX trickery.

*/
Molecule
Lookup::text (String style, String text, Paper_def *paper_l) const
{
  Molecule m;
  if (style.empty_b ())
    style = "roman";
  
  int font_mag = 1;
  Real font_h = paper_l->get_var ("font_normal");
  if (paper_l->scope_p_->elem_b ("font_" + style))
    {
      font_h = paper_l->get_var ("font_" + style);
    }
   
  if (paper_l->scope_p_->elem_b ("magnification_" + style))
    {
      font_mag = (int)paper_l->get_var ("magnification_" + style);
    }

  /*
    UGH.
  */
  SCM l = gh_eval_str (("(style-to-cmr \"" + style + "\")").ch_C());
  if (l != SCM_BOOL_F)
    {
      style = ly_scm2string (SCM_CDR(l)) +to_str  ((int)font_h);
    }

  Real w = 0;
  Interval ydims (0,0);

  Font_metric* afm_l = all_fonts_global_p->find_font (style);
  DOUT << "\nChars: ";


  int brace_count =0;
  for (int i = 0; i < text.length_i (); i++) 
    {
      
      if (text[i]=='\\') 
	{
	  for (i++; (i < text.length_i ()) && isalpha(text[i]); i++)
	    ;
	  i--; // Compensate for the increment in the outer loop!
	}
      else
	{
	  if (text[i] == '{')
	    brace_count ++;
	  else if (text[i] == '}')
	    brace_count --;
          Character_metric *c = afm_l->get_char ((unsigned char)text[i],false);

	  w += c->dimensions()[X_AXIS].length ();
	  ydims.unite (c->dimensions()[Y_AXIS]);
	}
    }

  if (font_mag > 1 && font_mag < 7 )
    {
      /* UGH  */ 
      style = style + String(" scaled \\magstep ") + to_str (font_mag);
      w *= mag_steps[font_mag];
      ydims *= mag_steps[font_mag];
    }

  if(brace_count)
    {
      warning (_f ("Non-matching braces in text `%s', adding braces.", text.ch_C()));

      if (brace_count < 0)
	{
	  text = to_str ('{', -brace_count) + text;
	}
      else 
	{
	  text = text + to_str ('}', brace_count);
	}
    }

  
  DOUT << "\n" << to_str (w) << "\n";
  m.dim_.x () = Interval (0, w);
  m.dim_.y () = ydims;

  
  Atom at  (gh_list (text_scm_sym,
		     gh_str02scm (text.ch_C()),
		     SCM_UNDEFINED));
  at.font_ = ly_symbol (style);
  
  m.add_atom (&at);
  return m;
}
  

Molecule
Lookup::time_signature (int num, int den, Paper_def *paper_l) const
{
  String sty = "number";
  Molecule n (text (sty, to_str (num), paper_l));
  Molecule d (text (sty, to_str (den), paper_l));
  n.align_to (X_AXIS, CENTER);
  d.align_to (X_AXIS, CENTER);
  Molecule m;
  if (den)
    {
      m.add_at_edge (Y_AXIS, UP, n, 0.0);
      m.add_at_edge (Y_AXIS, DOWN, d, 0.0);
    }
  else
    {
      m = n;
      m.align_to (Y_AXIS, CENTER);
    }
  return m;
}

Molecule
Lookup::staff_brace (Real y) const
{
  Molecule m;
  
  Atom at  (gh_list (pianobrace_scm_sym,
		     gh_double2scm (y),
		     SCM_UNDEFINED
		     ));
  
  m.dim_[Y_AXIS] = Interval (-y/2,y/2);
  m.dim_[X_AXIS] = Interval (0,0);
  m.add_atom (&at);
  return m;
}

Molecule
Lookup::hairpin (Real width, Real height, bool decresc, bool continued) const
{
  Molecule m;   

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
Lookup::tuplet_bracket (Real dy , Real dx, Real thick, Real interline_f, Direction dir) const
{
  Molecule m;

  Atom at  (gh_list(tuplet_scm_sym,
		    gh_double2scm (interline_f), 
		    gh_double2scm (dx),
		    gh_double2scm (dy),
		    gh_double2scm (thick),
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
  Atom at  ( gh_list (bracket_scm_sym,
		      gh_double2scm (y),
		      SCM_UNDEFINED));
  m.add_atom (&at);				 
  m.dim_[Y_AXIS] = Interval (-y/2,y/2);
  m.dim_[X_AXIS] = Interval (0,4 PT);

  m.translate_axis (- 4. / 3. * m.dim_[X_AXIS].length (), X_AXIS);
  return m;
}

Molecule
Lookup::volta (Real w, Real thick, Real interline_f, bool last_b) const
{
  Molecule m; 

  Atom at  (gh_list (volta_scm_sym,
		     gh_double2scm (w),
		     gh_double2scm (thick),
		     gh_int2scm (last_b),
		     SCM_UNDEFINED));

  m.dim_[Y_AXIS] = Interval (-interline_f, interline_f);
  m.dim_[X_AXIS] = Interval (0, w);

  m.add_atom (&at);
  return m;
}

Molecule
Lookup::accordion (SCM s, Real interline_f) const
{
  Molecule m;
  String sym = ly_scm2string(SCM_CAR(s));
  String reg = ly_scm2string(SCM_CAR(SCM_CDR(s)));

  if (sym == "Discant")
    {
      Molecule r = afm_find("scripts-accDiscant");
      m.add_molecule(r);
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 2.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      int eflag = 0x00;
      if (reg.left_str(3) == "EEE")
	{
	  eflag = 0x07;
	  reg = reg.right_str(reg.length_i()-3);
	}
      else if (reg.left_str(2) == "EE")
	{
	  eflag = 0x05;
	  reg = reg.right_str(reg.length_i()-2);
	}
      else if (reg.left_str(2) == "Eh")
	{
	  eflag = 0x04;
	  reg = reg.right_str(reg.length_i()-2);
	}
      else if (reg.left_str(1) == "E")
	{
	  eflag = 0x02;
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (eflag & 0x02)
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	}
      if (eflag & 0x04)
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 1.5 PT, Y_AXIS);
	  d.translate_axis(0.8 * interline_f PT, X_AXIS);
	  m.add_molecule(d);
	}
      if (eflag & 0x01)
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 1.5 PT, Y_AXIS);
	  d.translate_axis(-0.8 * interline_f PT, X_AXIS);
	  m.add_molecule(d);
	}
      if (reg.left_str(2) == "SS")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(0.5 * interline_f PT, Y_AXIS);
	  d.translate_axis(0.4 * interline_f PT, X_AXIS);
	  m.add_molecule(d);
	  d.translate_axis(-0.8 * interline_f PT, X_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-2);
	}
      if (reg.left_str(1) == "S")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(0.5 * interline_f PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
    }
  else if (sym == "Freebase")
    {
      Molecule r = afm_find("scripts-accFreebase");
      m.add_molecule(r);
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg == "E")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 0.5 PT, Y_AXIS);
	  m.add_molecule(d);
	}
    }
  else if (sym == "Bayanbase")
    {
      Molecule r = afm_find("scripts-accBayanbase");
      m.add_molecule(r);
      if (reg.left_str(1) == "T")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 2.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      /* include 4' reed just for completeness. You don't want to use this. */
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(2) == "EE")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 0.5 PT, Y_AXIS);
	  d.translate_axis(0.4 * interline_f PT, X_AXIS);
	  m.add_molecule(d);
	  d.translate_axis(-0.8 * interline_f PT, X_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-2);
	}
      if (reg.left_str(1) == "E")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 0.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
    }
  else if (sym == "Stdbase")
    {
      Molecule r = afm_find("scripts-accStdbase");
      m.add_molecule(r);
      if (reg.left_str(1) == "T")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 3.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "F")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 2.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "M")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 2 PT, Y_AXIS);
	  d.translate_axis(interline_f PT, X_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "E")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 1.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
      if (reg.left_str(1) == "S")
	{
	  Molecule d = afm_find("scripts-accDot");
	  d.translate_axis(interline_f * 0.5 PT, Y_AXIS);
	  m.add_molecule(d);
	  reg = reg.right_str(reg.length_i()-1);
	}
    }
  /* ugh maybe try to use regular font for S.B. and B.B and only use one font
     for the rectangle */
  else if (sym == "SB")
    {
      Molecule r = afm_find("scripts-accSB");
      m.add_molecule(r);
    }
  else if (sym == "BB")
    {
      Molecule r = afm_find("scripts-accBB");
      m.add_molecule(r);
    }
  else if (sym == "OldEE")
    {
      Molecule r = afm_find("scripts-accOldEE");
      m.add_molecule(r);
    }
  else if (sym == "OldEES")
    {
      Molecule r = afm_find("scripts-accOldEES");
      m.add_molecule(r);
    }
  return m;  
}
