/*   
  sustain-pedal.cc --  implement Sustain_pedal
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "grob.hh"
#include "molecule.hh"
#include "font-interface.hh"
#include "string.hh"

// update comment --hwn 
/*
  Urg.
  This is almost text
  Problem is:
    * we have no kerning
    * symbols are at wrong place in font



  Properties:

  glyph -- text string (TODO:   make one large glyph of the Ped symbol, removes need for member_brew_molecule ())

*/

struct Sustain_pedal
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
};


MAKE_SCHEME_CALLBACK (Sustain_pedal,brew_molecule,1);
SCM
Sustain_pedal::brew_molecule (SCM smob) 
{
  Grob * e = unsmob_grob (smob);
  
  Molecule mol;
  SCM glyph = e->get_grob_property ("text");
  if (!gh_string_p (glyph))
    return mol.smobbed_copy ();
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
      Molecule m = Font_interface::get_default_font (e)->find_by_name (idx);
      if (!m.empty_b ())
	mol.add_at_edge (X_AXIS, RIGHT, m, 0);
    }
    
  return mol.smobbed_copy ();
}

