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
 *
 * TODO: if properties font-family and style are not set properly
 * (e.g. by a user erronously setting font-family to #'music),
 * lilypond currently crashes with the message: "lilypond:
 * ../flower/include/interval.hh:28: Real Interval_t<double>::center()
 * const: Assertion `!empty_b ()' failed.".  The code of this class
 * should be clever enough to foresee a potential crash, print a
 * warning, and supply sensible default values that avoid the crash.
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

ADD_INTERFACE (Ligature_head,"ligature-head-interface","Ligature head",
	       "ligature-primitive-callback");
