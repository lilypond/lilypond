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
  /*
    UGH. Should work automatically via offset callback. 
   */
  Side_position_interface i (this);
  Direction d =  i.get_direction ();
  i.set_direction (d);
}

Molecule
Sustain_pedal::do_brew_molecule () const
{
  Molecule mol;
  SCM glyph = get_elt_property ("text");
  if (glyph == SCM_UNDEFINED)
    return mol;
  String text = ly_scm2string (glyph);

  for (int i = 0; i < text.length_i (); i++)
    {
      String idx = String ("pedal-") + String (&text.byte_C ()[i], 1);
      Molecule m = lookup_l ()->afm_find (idx);
      if (m.empty_b ())
	continue;
      Real kern = 0;
      if (i)
	{
	  SCM s = scm_eval (gh_list (ly_symbol2scm ("pedal-kerning"),
				     ly_str02scm (String (&text.byte_C ()[i - 1], 1).ch_C ()),
				     ly_str02scm (String (&text.byte_C ()[i], 1).ch_C ()),
				     SCM_UNDEFINED));
	  if (gh_number_p (s))
	    {
	      Staff_symbol_referencer_interface st (this);
	      Real staff_space = st.staff_space ();
	      kern = gh_scm2double (s) * staff_space;
	    }
	}
      mol.add_at_edge (X_AXIS, RIGHT, m, kern);
    }
    
  return mol;
}


