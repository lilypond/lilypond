/*
  paper-column.cc -- implement Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "moment.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "warn.hh"
#include "axis-group-interface.hh"
#include "spaceable-grob.hh"
#include "molecule.hh"
#include "text-item.hh"
#include "lookup.hh"
#include "font-interface.hh"





ADD_INTERFACE (Paper_column, "paper-column-interface",
  "  Paper_columns form the top-most item parent. (The Paper_columns X
  parent is System, which is a spanner.)

  Paper_columns form the units for the spacing engine. They are
  numbered, the first (leftmost) is column 0. Numbering happens before
  line-breaking, and columns are not renumbered after line breaking.

  Since many columns go unused, you should only use the rank field to
  get ordering information.  Two adjacent columns may have
  non-adjacent numbers.

  Don't be confused by right-items: each spacing wish can also contain
  a number of items, with which a spacing constraint may be kept. It's
  a little baroque, but it might come in handy later on?

",
  "between-cols between-system-string when bounded-by-me shortest-playing-duration shortest-starter-duration");


void
Paper_column::do_break_processing ()
{
  Spaceable_grob::remove_interface (this);
  Item::do_break_processing ();
}


int
Paper_column::get_rank (Grob*me) 
{
  return dynamic_cast<Paper_column*> (me)->rank_;
}

System*
Paper_column::get_system () const
{
  return system_;
}

Paper_column*
Paper_column::get_column () const
{
  return (Paper_column*) (this);
}

Paper_column::Paper_column (SCM l)
  : Item (l)		// guh.?
{
  system_=0;
  rank_ = -1;
}

Moment
Paper_column::when_mom (Grob*me)
{
  SCM m = me->get_grob_property ("when");
  Moment s (0);
  if (unsmob_moment (m))
    {
      return *unsmob_moment (m);
    }
  return s;
}

bool
Paper_column::musical_b (Grob *me)
{
  SCM m = me->get_grob_property ("shortest-starter-duration");
  Moment s (0);
  if (unsmob_moment (m))
    {
      s = *unsmob_moment (m);
    }
  return s != Moment (0);
  
}
  

bool
Paper_column::used_b (Grob*me)
{
  return gh_pair_p (me->get_grob_property ("elements")) ||  Item::breakable_b (me)
    || gh_pair_p (me->get_grob_property ("bounded-by-me"))
    ;
}

/*
  Print a vertical line and  the rank number, to aid debugging.  
 */

MAKE_SCHEME_CALLBACK(Paper_column,brew_molecule,1);
SCM
Paper_column::brew_molecule (SCM p)
{
  Grob *me = unsmob_grob (p);

  String r = to_string (Paper_column::get_rank (me));
  SCM properties = Font_interface::font_alist_chain (me);
  
  Molecule t = Text_item::text2molecule (me, ly_str02scm (r.to_str0 ()),
					 properties);
  t.align_to (X_AXIS, CENTER);
  t.align_to (Y_AXIS, DOWN);
  
  Molecule l = Lookup::filledbox (Box (Interval (-0.01, 0.01),
				       Interval (-2, -1)));

  t.add_molecule (l);
  return t.smobbed_copy ();						
}

/*
  This is all too hairy. We use bounded-by-me to make sure that some
  columns are kept "alive". Unfortunately, when spanners are suicided,
  this falls apart again. (sigh.)

  THIS IS BROKEN KLUDGE. WE SHOULD INVENT SOMETHING BETTER. 
 */
MAKE_SCHEME_CALLBACK(Paper_column,before_line_breaking,1);
SCM
Paper_column::before_line_breaking (SCM grob)
{
  Grob *me = unsmob_grob (grob);

  SCM c = me->get_grob_property ("bounded-by-me");
  SCM *ptrptr = &c;

  while (gh_pair_p (*ptrptr))
    {
      Grob * g = unsmob_grob (gh_car (*ptrptr));

      if (!g || !g->live ())
	{
	  *ptrptr = gh_cdr (*ptrptr);
	}
      else
	{
	  ptrptr = SCM_CDRLOC (*ptrptr);
	}
    }

  me->set_grob_property ("bounded-by-me", c);
  return SCM_UNSPECIFIED;
}
