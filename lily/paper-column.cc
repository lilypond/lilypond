/*
  paper-column.cc -- implement Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "moment.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "debug.hh"
#include "axis-group-interface.hh"
#include "spaceable-grob.hh"
#include "molecule.hh"
#include "text-item.hh"
#include "lookup.hh"
#include "font-interface.hh"


/*
  Paper_columns form the top-most item parent. (The Paper_columns X
  parent is System, which is a spanner.)

  Paper_columns form the units for the spacing engine. They are
  numbered, the first (leftmost) is column 0. Numbering happens before
  line-breaking, and columns are not renumbered after line breaking.

  Since many columns go unused, you should only use the rank field to
  get ordering information.  Two adjacent columns may have
  non-adjacent numbers.
  
 */

void
Paper_column::do_break_processing ()
{
  Spaceable_grob::remove_interface (this);
  Item::do_break_processing ();
}

int
Paper_column::rank_i (Grob*me) 
{
  return dynamic_cast<Paper_column*> (me)->rank_i_;
}

System*
Paper_column::line_l () const
{
  return line_l_;
}

Paper_column*
Paper_column::column_l () const
{
  return (Paper_column*) (this);
}

Paper_column::Paper_column (SCM l)
  : Item (l)		// guh.?
{
  line_l_=0;
  rank_i_ = -1;
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

  String r = to_str (Paper_column::rank_i (me));
  SCM properties = Font_interface::font_alist_chain (me);
  
  Molecule t = Text_item::text2molecule (me, ly_str02scm (r.ch_C()),
					 properties);
  t.align_to (X_AXIS, CENTER);
  t.align_to (Y_AXIS, DOWN);
  
  Molecule l = Lookup::filledbox (Box (Interval (-0.01, 0.01),
				       Interval (-2, -1)));

  t.add_molecule (l);
  return t.smobbed_copy ();						
}




ADD_INTERFACE (Paper_column, "paper-column-interface",
  "",
  "between-cols count between-system-string when bounded-by-me shortest-playing-duration shortest-starter-duration");
