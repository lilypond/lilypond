/*
  ligature-head.cc -- implement Ligature_head

  source file of the GNU LilyPond music typesetter

  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "ligature-head.hh"
#include "item.hh"
#include "note-head.hh"
#include "warn.hh"

/*
 * TODO: in scm/grob-description.scm, LigatureHead must contain value
 * "rhythmic-head-interface" in the interfaces list.  Otherwise, text
 * scripts (such as fermata) are horizontally aligned with the end of
 * the ligature rather than with the associated head.  Why?
 */
MAKE_SCHEME_CALLBACK (Ligature_head,brew_molecule,1);
SCM
Ligature_head::brew_molecule (SCM smob)  
{
  Grob *me = unsmob_grob (smob);
  SCM brew_ligature_primitive_proc =
    me->get_grob_property ("ligature-primitive-callback");
  if (brew_ligature_primitive_proc != SCM_EOL)
    {
      return gh_call1 (brew_ligature_primitive_proc, smob);
    }
  else
    {
      warning ("Ligature_head: ligature-primitive-callback undefined -> resorting to Note_head::brew_molecule");
      return Note_head::brew_molecule (smob);
    }
}

/*
  UGH  primitive is only used within the engraver.
*/
ADD_INTERFACE (Ligature_head,"ligature-head-interface","Ligature head",
	       "primitive ligature-primitive-callback thickness flexa-width head-width delta-pitch join-left");
