/*   
  note-spacing.cc -- implement Note_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "paper-column.hh"
#include "item.hh"
#include "moment.hh"
#include "note-spacing.hh"
#include "grob.hh"
#include "note-column.hh"
#include "warn.hh"

bool
Note_spacing::has_interface (Grob* g)
{
  return g && g->has_interface (ly_symbol2scm ("note-spacing-interface"));
}



Real
Note_spacing::get_spacing (Grob *me)
{
  Drul_array<SCM> props(me->get_grob_property ("left-items"),
			me->get_grob_property ("right-items"));
  Direction d = LEFT;
  Drul_array<Interval> extents;
  do
    {
      for (SCM  s = props[d]; gh_pair_p (s); s = gh_cdr (s))
	{
	  Item * it= dynamic_cast<Item*> (unsmob_grob (gh_car(s)));
	  extents[d].unite (it->extent (it->column_l (), X_AXIS));

	  if (d == RIGHT)
	    {
	      Grob * accs = Note_column::accidentals (it);
	      if (accs)
		extents[d].unite (accs->extent (it->column_l (), X_AXIS));
	    }
	}

      if (extents[d].empty_b ())
	extents[d] = Interval (0,0);
    }
  while (flip (&d) != LEFT);

  /*
    
    What's sticking out at the left of the right side has less
    influence.

  */
  Real dx= extents[LEFT][RIGHT] - 0.5 * extents[RIGHT][LEFT];
  return dx;
}


MAKE_SCHEME_CALLBACK(Note_spacing, before_line_breaking, 1)
SCM
Note_spacing::before_line_breaking (SCM g)
{
  Grob * me = unsmob_grob (g);
  SCM right = me->get_grob_property ("right-items");

  if (gh_pair_p (right))
    right = gh_car (right);
  
  Grob *right_grob = unsmob_grob (right);

  Item * ri = dynamic_cast<Item*> (right_grob);
  if (!ri)
    {
      int r = Paper_column::rank_i (dynamic_cast<Item*>(me)->column_l ());
      programming_error (_f("Spacing wish column %d has no right item.", r));
    }
  else
    {
      me->set_grob_property ("right-column", ri->column_l ()->self_scm());
    }
  
  return SCM_UNSPECIFIED;
}
