/*
  chord-name.cc -- implement Chord_name

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord-name.hh"

#include "output-def.hh"
#include "font-interface.hh"
#include "paper-column.hh"
#include "system.hh"
#include "staff-symbol-referencer.hh"
#include "text-interface.hh"

MAKE_SCHEME_CALLBACK (Chord_name, after_line_breaking, 1);
SCM
Chord_name::after_line_breaking (SCM smob)
{
  Item *me = unsmob_item (smob);
  assert (me);

  SCM s = me->get_property ("begin-of-line-visible");
  if (to_boolean (s))
    {
      if (me->get_column ()->get_rank ()
	  - me->get_system ()->spanned_rank_interval ()[LEFT] > 1)
	me->suicide ();
    }
  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Chord_name,
	       "A chord name.",

	       /* properties */	       
	       "begin-of-line-visible "
	       );
