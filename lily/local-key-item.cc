/*
  local-key-item.cc -- implement Local_key_item, Pitch

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "local-key-item.hh"
#include "molecule.hh"
#include "staff-symbol-referencer.hh"
#include "font-interface.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "rhythmic-head.hh"
#include "misc.hh"
#include "spanner.hh"
#include "tie.hh"
#include "lookup.hh"

static SCM
pitch_less (SCM p1, SCM p2)
{
  return Pitch::less_p (ly_car (p1),  ly_car (p2));
}

static SCM pitch_less_proc;

void
init_pitch_funcs ()
{
  pitch_less_proc = gh_new_procedure2_0 ("pitch-less", &pitch_less);
}

ADD_SCM_INIT_FUNC (lkpitch,init_pitch_funcs);


void
Local_key_item::add_pitch (Grob*me, Pitch p, bool cautionary, bool natural,
			   Grob* tie_break_reminder)
{
  SCM acs = me->get_grob_property ("accidentals");
  SCM pitch = p.smobbed_copy ();
  SCM opts = scm_assoc (pitch, acs);
  bool new_pitch = !gh_pair_p (opts);
  opts= new_pitch ? SCM_EOL : gh_cdr (opts);
  
  if (cautionary)
    opts = gh_cons (ly_symbol2scm ("cautionary"), opts);
  if (natural)
    opts = gh_cons (ly_symbol2scm ("natural"), opts);
  if (tie_break_reminder)
    {
      /* Ugh, these 'options' can't have a value, faking... */
      opts = gh_cons (tie_break_reminder->self_scm (), opts);
      opts = gh_cons (ly_symbol2scm ("tie-break-reminder"), opts);
    }

  if (new_pitch)
    {
      pitch = gh_cons (pitch, opts);
      acs = scm_merge_x (acs, gh_cons (pitch, SCM_EOL), pitch_less_proc);
    }
  else
    scm_assoc_set_x (acs, pitch, opts);

  me->set_grob_property ("accidentals", acs);
}

Molecule
Local_key_item::parenthesize (Grob*me, Molecule m)
{
  Molecule open = Font_interface::get_default_font (me)->find_by_name (String ("accidentals-leftparen"));
  Molecule close = Font_interface::get_default_font (me)->find_by_name (String ("accidentals-rightparen"));
  m.add_at_edge (X_AXIS, LEFT, Molecule (open), 0);
  m.add_at_edge (X_AXIS, RIGHT, Molecule (close), 0);

  return m;
}

/*
  HW says: maybe move to tie.cc

  Note, tie should not kill all accidentals when broken, only the ones
  that are indicated by a property tie-break-reminder, I guess

  Find if any of the accidentals were created because they're at the rhs of a
  tie.  If that's the reason they exist, and the tie was NOT broken,
  put the accidental up for deletion.  Clear molecule cache. */
MAKE_SCHEME_CALLBACK (Local_key_item, after_line_breaking, 1);
SCM
Local_key_item::after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  SCM accs = me->get_grob_property ("accidentals");
  for (SCM s = accs;
	gh_pair_p (s); s = ly_cdr (s))
    {
      SCM opts = ly_cdar (s);

      SCM t = scm_memq (ly_symbol2scm ("tie-break-reminder"), opts);
      if (t != SCM_BOOL_F)
	{
	  Grob *tie = unsmob_grob (ly_cadr (t));
	  Spanner *sp = dynamic_cast<Spanner*> (tie);
	  if (!sp->original_l_)
	    {
	      /* there should be a better way to delete part of me */
	      scm_set_car_x (s, scm_list_n (ly_caar (s),
					 ly_symbol2scm ("deleted"),
					 SCM_UNDEFINED));
	      me->set_grob_property ("molecule", SCM_EOL);
	    }
	}
    }
  
  return SCM_UNSPECIFIED;
}

/*
  UGH. clean me, revise placement routine (See Ross & Wanske;
  accidental placement is more complicated than this.
 */

MAKE_SCHEME_CALLBACK (Local_key_item,brew_molecule,1);
SCM
Local_key_item::brew_molecule (SCM smob)
{
  Grob* me = unsmob_grob (smob);
  
  Molecule mol;

  Real note_distance = Staff_symbol_referencer::staff_space (me)/2;
  Molecule octave_mol;
  bool oct_b = false;
  int lastoct = -100;

  SCM scm_style = me->get_grob_property ("style");
  String style;
  if (gh_symbol_p (scm_style))
    {
      style = ly_scm2string (scm_symbol_to_string (scm_style));
    }
  else
    {
      /*
	preferably no name for the default style.
       */
      style = "";
    }

  SCM accs = me->get_grob_property ("accidentals");
  for (SCM s = accs;
	gh_pair_p (s); s = ly_cdr (s))
    {
      Pitch p (*unsmob_pitch (ly_caar (s)));
      SCM opts = ly_cdar (s);
      
      if (scm_memq (ly_symbol2scm ("deleted"), opts) != SCM_BOOL_F)
	continue;
      
      // do one octave
      if (p.octave_i ()  != lastoct) 
	{
	  if (oct_b)
	    {
	      Real dy =lastoct*7* note_distance;
	      octave_mol.translate_axis (dy, Y_AXIS);
	      mol.add_molecule (octave_mol);
	      octave_mol = Molecule ();
	    }
	  oct_b = true; 
	}
      

      lastoct = p.octave_i () ;

      bool cautionary = (scm_memq (ly_symbol2scm ("cautionary"), opts) != SCM_BOOL_F);
      SCM font_rel_siz = me->get_grob_property("font-relative-size");
      SCM caut_siz = me->get_grob_property("cautionary-size");
      int frs = (gh_exact_p(font_rel_siz) ? gh_scm2int(font_rel_siz) : 0);
      int cs = (gh_exact_p(caut_siz) ? gh_scm2int(caut_siz) : 0);


      // Ugh. This will only work if only called once on each grob. --rz
      if (cautionary && caut_siz!=0)
	me->set_grob_property ("font-relative-size",gh_int2scm(frs+cs));

      SCM c0 =  me->get_grob_property ("c0-position");
      Real dy = (gh_number_p (c0) ? gh_scm2int (c0) : 0 + p.notename_i_)
	* note_distance;

      Molecule acc (Font_interface::get_default_font (me)->
		    find_by_name (String ("accidentals-") +
				  style +
				  to_str (p.alteration_i_)));
      
      if (scm_memq (ly_symbol2scm ("natural"), opts) != SCM_BOOL_F)
	{
	  Molecule prefix = Font_interface::get_default_font (me)->
	      find_by_name (String ("accidentals-") + style + String ("0"));
	  acc.add_at_edge (X_AXIS, LEFT, Molecule (prefix), 0);
	}

      if (cautionary && to_boolean(me->get_grob_property("paren-cautionaries")))
        acc = parenthesize (me, acc);

      acc.translate_axis (dy, Y_AXIS);
      octave_mol.add_at_edge (X_AXIS, RIGHT, acc, 0);
    }

  if (oct_b)
    {
      Real dy =lastoct*7*note_distance;
      octave_mol.translate_axis (dy, Y_AXIS);
      mol.add_molecule (octave_mol);
      octave_mol = Molecule ();
    }
  
 if (gh_pair_p (accs))
    {
      Drul_array<SCM> pads;

      /*
	Use a cons?
       */
      pads[RIGHT] = me->get_grob_property ("right-padding");
      pads[LEFT] = me->get_grob_property ("left-padding");


      // unused ?
      Direction d = LEFT;
      do {
	if (!gh_number_p (pads[d]))
	  continue;

	Box b (Interval (0, gh_scm2double (pads[d]) * note_distance),
	      Interval (0,0));
	Molecule m (Lookup::blank (b));
	mol.add_at_edge (X_AXIS, d, m, 0);
      } while (flip (&d)!= LEFT);
    }

  return mol.smobbed_copy ();
}




ADD_INTERFACE (Local_key_item, "accidentals-interface",
  "Accidentals",
  "accidentals left-padding right-padding paren-cautionaries cautionary-size");
