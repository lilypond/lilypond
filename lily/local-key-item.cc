/*
  local-key-item.cc -- implement Local_key_item, Pitch

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "local-key-item.hh"
#include "molecule.hh"
#include "staff-symbol-referencer.hh"
#include "font-interface.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "rhythmic-head.hh"
#include "misc.hh"
#include "lookup.hh"

static SCM
pitch_less  (SCM p1, SCM p2)
{
  return Pitch::less_p (gh_car (p1),  gh_car (p2));
}

static SCM pitch_less_proc;

void
init_pitch_funcs ()
{
  pitch_less_proc = gh_new_procedure2_0 ("pits-less", &pitch_less);
}

ADD_SCM_INIT_FUNC(lkpitch,init_pitch_funcs);


void
Local_key_item::add_pitch (Grob*me, Pitch p, bool cautionary, bool natural)
{
  SCM acs = me->get_grob_property ("accidentals");
  SCM pitch = p.smobbed_copy ();
  SCM opts = SCM_EOL;
  if (cautionary)
    opts = gh_cons (ly_symbol2scm ("cautionary"), opts);
  if (natural)
    opts = gh_cons (ly_symbol2scm ("natural"), opts);

  pitch = gh_cons (pitch, opts);
  acs = scm_merge_x (acs, gh_cons (pitch, SCM_EOL), pitch_less_proc);

  me->set_grob_property ("accidentals", acs);
}

Molecule
Local_key_item::parenthesize (Grob*me, Molecule m)
{
  Molecule open = Font_interface::get_default_font (me)->find_by_name (String ("accidentals-("));
  Molecule close = Font_interface::get_default_font (me)->find_by_name (String ("accidentals-)"));
  m.add_at_edge(X_AXIS, LEFT, Molecule(open), 0);
  m.add_at_edge(X_AXIS, RIGHT, Molecule(close), 0);

  return m;
}

/*
  UGH. clean me, revise placement routine (See Ross & Wanske;
  accidental placement is more complicated than this.
 */

MAKE_SCHEME_CALLBACK(Local_key_item,brew_molecule,1);
SCM
Local_key_item::brew_molecule (SCM smob)
{
  Grob* me = unsmob_element (smob);
  
  Molecule mol;

  Real note_distance = Staff_symbol_referencer::staff_space (me)/2;
  Molecule octave_mol;
  bool oct_b = false;
  int lastoct = -100;

  SCM accs = me->get_grob_property ("accidentals");
  for  (SCM s = accs;
	gh_pair_p (s); s = gh_cdr (s))
    {
      Pitch p (*unsmob_pitch (gh_caar (s)));
      SCM opts = gh_cdar (s);
      
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

      SCM c0 =  me->get_grob_property ("c0-position");
      Real dy = (gh_number_p (c0) ? gh_scm2int (c0) : 0 + p.notename_i_)
	* note_distance;
      
      Molecule acc (Font_interface::get_default_font (me)->find_by_name (String ("accidentals-")
					       + to_str (p.alteration_i_)));
      
      if (scm_memq (ly_symbol2scm ("natural"), opts) != SCM_BOOL_F)
	{
	  Molecule prefix = Font_interface::get_default_font (me)->find_by_name (String ("accidentals-0"));
	  acc.add_at_edge(X_AXIS, LEFT, Molecule(prefix), 0);
	}

      if (scm_memq (ly_symbol2scm ("cautionary"), opts) != SCM_BOOL_F)
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

	Box b(Interval (0, gh_scm2double (pads[d]) * note_distance),
	      Interval (0,0));
	Molecule m (Lookup::blank (b));
	mol.add_at_edge (X_AXIS, d, m, 0);
      } while ( flip (&d)!= LEFT);
    }

  return mol.smobbed_copy();
}

bool
Local_key_item::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("accidentals-interface"));
}
void
Local_key_item::set_interface (Grob*m)
{
  m->set_interface (ly_symbol2scm ("accidentals-interface"));
}
