/*   
  sustain-pedal.cc --  implement Sustain_pedal
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "sustain-pedal.hh"
#include "side-position-interface.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"

void
Sustain_pedal::after_line_breaking ()
{
  return ;
  /*
    UGH. Should work automatically via offset callback. 
   */
  Side_position_interface i (this);
  Direction d =  i.get_direction ();
  i.set_direction (d);
}

MAKE_SCHEME_SCORE_ELEMENT_NON_DEFAULT_CALLBACKS(Sustain_pedal);


SCM
Sustain_pedal::scheme_molecule (SCM smob) 
{
  Score_element * e = unsmob_element (smob);
  
  Molecule mol;
  SCM glyph = e->get_elt_property ("text");
  if (!gh_string_p (glyph))
    return mol.create_scheme();
  String text = ly_scm2string (glyph);

  for (int i = 0; i < text.length_i (); i++)
    {
      String idx ("pedal-");
      if (text.cut_str (i, 3) == "Ped")
	{
	  idx += "Ped";
	  i += 2;
	}
      else
	idx += String (&text.byte_C ()[i], 1);
      Molecule m = e->lookup_l ()->afm_find (idx);
      if (!m.empty_b ())
	mol.add_at_edge (X_AXIS, RIGHT, m, 0);
    }
    
  return mol.create_scheme ();
}

Sustain_pedal ::Sustain_pedal(SCM s )
  : Item (s)
{
}
