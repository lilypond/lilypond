/*
  local-key-item.cc -- implement Local_key_item, Musical_pitch

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "local-key-item.hh"
#include "molecule.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "rhythmic-head.hh"
#include "misc.hh"

void
Local_key_item::add_pitch (Musical_pitch p, bool cautionary, bool natural)
{
  for (int i=0; i< accidental_arr_.size(); i++)
    if (!Musical_pitch::compare (p, accidental_arr_[i].pitch_))
      return;
	  /* maybe natural (and cautionary) should be modif. nonetheless? */

  Local_key_cautionary_tuple t;
  t.pitch_ = p;
  t.cautionary_b_ = cautionary;
  t.natural_b_ = natural;
  accidental_arr_.push (t);
}

MAKE_SCHEME_CALLBACK(Local_key_item,before_line_breaking);

SCM
Local_key_item::before_line_breaking (SCM smob)
{
  Local_key_item* me = dynamic_cast<Local_key_item*>(unsmob_element (smob));
  me->accidental_arr_.sort (Local_key_cautionary_tuple::compare);
  return SCM_UNSPECIFIED;
}

Molecule
Local_key_item::accidental (Score_element*me, int j, bool cautionary, bool natural) 
{
  Molecule m (me->lookup_l ()->afm_find (String ("accidentals-") + to_str (j)));
  if (natural)
    {
      Molecule prefix = me->lookup_l ()->afm_find (String ("accidentals-0"));
      m.add_at_edge(X_AXIS, LEFT, Molecule(prefix), 0);
    }
  if (cautionary) 
    {
      Molecule open = me->lookup_l ()->afm_find (String ("accidentals-("));
      Molecule close = me->lookup_l ()->afm_find (String ("accidentals-)"));
      m.add_at_edge(X_AXIS, LEFT, Molecule(open), 0);
      m.add_at_edge(X_AXIS, RIGHT, Molecule(close), 0);
    }
  
  return m;
}

/*
  UGH. clean me up
 */

MAKE_SCHEME_CALLBACK(Local_key_item,brew_molecule);
SCM
Local_key_item::brew_molecule (SCM smob)
{
  Local_key_item* lki = dynamic_cast<Local_key_item*>(unsmob_element (smob));
  Score_element* me = lki;
  
  Molecule mol;

  Real note_distance = Staff_symbol_referencer::staff_space (me)/2;
  Molecule octave_mol;
  bool oct_b = false;
  int lastoct = -100;
  
  for  (int i = 0; i < lki->accidental_arr_.size(); i++) 
    {
      Musical_pitch p (lki->accidental_arr_[i].pitch_);
      // do one octave
      if (p.octave_i_ != lastoct) 
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
      
      lastoct = p.octave_i_;

      SCM c0 =  me->get_elt_property ("c0-position");
      Real dy = (gh_number_p (c0) ? gh_scm2int (c0) : 0 + p.notename_i_)
	* note_distance;
      
      Molecule m (accidental (me,p.accidental_i_,
			      lki->accidental_arr_[i].cautionary_b_,
			      lki->accidental_arr_[i].natural_b_));

      m.translate_axis (dy, Y_AXIS);
      octave_mol.add_at_edge (X_AXIS, RIGHT, m, 0);
    }

  if (oct_b)
    {
      Real dy =lastoct*7*note_distance;
      octave_mol.translate_axis (dy, Y_AXIS);
      mol.add_molecule (octave_mol);
      octave_mol = Molecule ();
    }
  
 if (lki->accidental_arr_.size()) 
    {
      Drul_array<SCM> pads;

      /*
	Use a cons?
       */
      pads[RIGHT] = me->get_elt_property ("right-padding");
      pads[LEFT] = me->get_elt_property ("left-padding");


      // unused ?
      Direction d = LEFT;
      do {
	if (!gh_number_p (pads[d]))
	  continue;

	Box b(Interval (0, gh_scm2double (pads[d]) * note_distance),
	      Interval (0,0));
	Molecule m (me->lookup_l ()->blank (b));
	mol.add_at_edge (X_AXIS, d, m, 0);
      } while ( flip (&d)!= LEFT);
    }

  return mol.create_scheme();
}

Local_key_item::Local_key_item (SCM s)
  : Item (s)
{
  
}
bool
Local_key_item::has_interface (Score_element*m)
{
  return m && m->has_interface (ly_symbol2scm ("accidentals-interface"));
}
