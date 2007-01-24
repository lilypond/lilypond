/*
  separating-group-spanner.cc -- implement Separating_group_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "separating-group-spanner.hh"

#include "separation-item.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "dimensions.hh"
#include "pointer-group-interface.hh"
#include "moment.hh"

void
Separating_group_spanner::find_rods (Item *r,
				     vector<Grob*> const &separators,
				     vsize idx,
				     Real padding)
{

  /*
    This is an inner loop: look for the first normal (unbroken) Left
    grob.  This looks like an inner loop (ie. quadratic total), but in
    most cases, the interesting L will just be the first entry of
    NEXT, making it linear in most of the cases.
  */
  for (; idx != VPOS; idx--)
    {
      Item *l = dynamic_cast<Item *> (separators[idx]);
      Item *lb = l->find_prebroken_piece (RIGHT);

      if (lb)
	{
	  Separation_item::set_distance (Drul_array<Item*> (lb, r), padding);
	}

      if (Separation_item::set_distance (Drul_array<Item *> (l, r), padding)
	  /*
	    This check is because grace notes are set very tight, and
	    the accidentals of main note may stick out so far to cover
	    a barline preceding the grace note.
	   */
	  && spanned_time_interval (l, r).length ().main_part_ > Rational (0))
	break;

      /*
	this grob doesn't cause a constraint. We look further until we
	find one that does.
      */
    }
}

MAKE_SCHEME_CALLBACK (Separating_group_spanner, set_spacing_rods, 1);
SCM
Separating_group_spanner::set_spacing_rods (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /*
    Ugh: padding is added doubly, also for SeparationItem
  */
  Real padding = robust_scm2double (me->get_property ("padding"), 0.1);

  extract_grob_set (me, "elements", elts);
  for (vsize i = elts.size (); i-- > 1;)
    {
      Item *r = dynamic_cast<Item *> (elts[i]);
      if (!r)
	continue;

      if (Separation_item::width (r).is_empty ())
	continue;

      Item *rb
	= dynamic_cast<Item *> (r->find_prebroken_piece (LEFT));

      find_rods (r, elts, i - 1, padding);
      if (rb)
	find_rods (rb, elts, i - 1, padding);
    }

  return SCM_UNSPECIFIED;
}

void
Separating_group_spanner::add_spacing_unit (Grob *me, Item *i)
{
  Pointer_group_interface::add_unordered_grob (me, ly_symbol2scm ("elements"), i);
}

ADD_INTERFACE (Separating_group_spanner,
	       "A spanner that calculates spacing constraints (\"rods\") "
	       "using the @code{separation-item-interface} grobs in @code{elements}.",

	       /* properties */
	       "elements "
	       "padding ");
