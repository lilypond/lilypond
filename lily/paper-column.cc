/*
  paper-column.cc -- implement Paper_column

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "moment.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "warn.hh"
#include "axis-group-interface.hh"
#include "spaceable-grob.hh"
#include "stencil.hh"
#include "text-item.hh"
#include "lookup.hh"
#include "font-interface.hh"
#include "paper-def.hh"




ADD_INTERFACE (Paper_column, "paper-column-interface",
	       "@code{Paper_column} objects form the top-most X-parents for items. "
	       "  The are two types of columns: musical columns, where are attached to, and "
	       "  non-musical columns, where bar-lines, clefs etc. are attached to. "
	       "  The spacing engine determines the X-positions of these objects."
	       "\n\n"
	       "They are\n"
	       "  numbered, the first (leftmost) is column 0. Numbering happens before\n"
	       "  line-breaking, and columns are not renumbered after line breaking.\n"
	       "  Since many columns go unused, you should only use the rank field to\n"
	       "  get ordering information.  Two adjacent columns may have\n"
	       "  non-adjacent numbers.\n"
	       "\n"
	       ,
	       "between-cols between-system-string when bounded-by-me "
	       "shortest-playing-duration shortest-starter-duration");

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
  SCM m = me->get_property ("when");
  Moment s (0);
  if (unsmob_moment (m))
    {
      return *unsmob_moment (m);
    }
  return s;
}

bool
Paper_column::is_musical (Grob *me)
{
  SCM m = me->get_property ("shortest-starter-duration");
  Moment s (0);
  if (unsmob_moment (m))
    {
      s = *unsmob_moment (m);
    }
  return s != Moment (0);
}
  

bool
Paper_column::is_used (Grob*me)
{
  return is_pair (me->get_property ("elements")) ||  Item::is_breakable (me)
    || is_pair (me->get_property ("bounded-by-me"))
    ;
}

/*
  Print a vertical line and  the rank number, to aid debugging.  
 */

MAKE_SCHEME_CALLBACK (Paper_column,print,1);
SCM
Paper_column::print (SCM p)
{
  Grob *me = unsmob_grob (p);

  String r = to_string (Paper_column::get_rank (me));
  SCM properties = Font_interface::text_font_alist_chain (me);

  SCM scm_mol = Text_item::interpret_markup (me->get_paper ()->self_scm (),
					     properties,
					     scm_makfrom0str (r.to_str0 ()));
  Stencil t = *unsmob_stencil (scm_mol);
  t.align_to (X_AXIS, CENTER);
  t.align_to (Y_AXIS, DOWN);
  
  Stencil l = Lookup::filled_box (Box (Interval (-0.01, 0.01),
				       Interval (-2, -1)));

  t.add_stencil (l);
  return t.smobbed_copy ();						
}

/*
  This is all too hairy. We use bounded-by-me to make sure that some
  columns are kept "alive". Unfortunately, when spanners are suicided,
  this falls apart again. (sigh.)

  THIS IS BROKEN KLUDGE. WE SHOULD INVENT SOMETHING BETTER. 
 */
MAKE_SCHEME_CALLBACK (Paper_column,before_line_breaking,1);
SCM
Paper_column::before_line_breaking (SCM grob)
{
  Grob *me = unsmob_grob (grob);

  SCM c = me->get_property ("bounded-by-me");
  SCM *ptrptr = &c;

  while (is_pair (*ptrptr))
    {
      Grob * g = unsmob_grob (ly_car (*ptrptr));

      if (!g || !g->live ())
	{
	  *ptrptr = ly_cdr (*ptrptr);
	}
      else
	{
	  ptrptr = SCM_CDRLOC (*ptrptr);
	}
    }

  me->set_property ("bounded-by-me", c);
  return SCM_UNSPECIFIED;
}
