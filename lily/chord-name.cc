/*
  chord-name.cc -- implement Chord_name

  source file of the GNU LilyPond music typesetter

  (c) 1999--2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord-name.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "paper-column.hh"
#include "system.hh"
#include "staff-symbol-referencer.hh"
#include "text-item.hh"

MAKE_SCHEME_CALLBACK (Chord_name,after_line_breaking,1);
SCM
Chord_name::after_line_breaking (SCM smob)
{
  Item* me = unsmob_item (smob);
  assert (me);
    
  SCM s = me->get_grob_property ("begin-of-line-visible");
  if (to_boolean (s))
    {
      if (Paper_column::get_rank (me->get_column ()) -
	  me->get_system ()->spanned_rank_iv ()[LEFT] > 1)
	me->suicide ();
    }
  return SCM_UNSPECIFIED;
}


ADD_INTERFACE (Chord_name, "chord-name-interface",
  "generate a chord name",
  "pitches chord style inversion bass begin-of-line-visible");

