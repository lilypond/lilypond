/*   
  separating-group-spanner.cc --  implement Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "separation-item.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "dimensions.hh"
#include "group-interface.hh"

void
Separating_group_spanner::find_rods (Item * r, SCM next)
{
  Interval ri (Separation_item::my_width (r));
  if (ri.empty_b ())
    return;

  /*
    This is an inner loop, however, in most cases, the interesting L
    will just be the first entry of NEXT, making it linear in most of
    the cases.  */
  for(; gh_pair_p (next); next = ly_cdr (next))
    {
      Item *l = dynamic_cast<Item*> (unsmob_grob (ly_car( next)));
      Item *lb = l->find_prebroken_piece (RIGHT);

      if (lb)
	{
	  Interval li (Separation_item::my_width (lb));

	  if (!li.empty_b ())
	    {
	      Rod rod;

	      rod.item_l_drul_[LEFT] = lb;
	      rod.item_l_drul_[RIGHT] = r;

	      rod.distance_f_ = li[RIGHT] - ri[LEFT];
	
	      rod.columnize ();
	      rod.add_to_cols ();
	    }
	}

      Interval li (Separation_item::my_width (l));
      if (!li.empty_b ())
	{
	  Rod rod;

	  rod.item_l_drul_[LEFT] =l;
	  rod.item_l_drul_[RIGHT]=r;

	  rod.distance_f_ = li[RIGHT] - ri[LEFT];
	
	  rod.columnize ();
	  rod.add_to_cols ();

	  break;
	}
      else
	/*
	  this grob doesn't cause a constraint. We look further until we
	  find one that does.  */
	;
    }
}

MAKE_SCHEME_CALLBACK (Separating_group_spanner,set_spacing_rods_and_seqs,1);
SCM
Separating_group_spanner::set_spacing_rods_and_seqs (SCM smob)
{
  set_spacing_rods (smob);

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Separating_group_spanner,set_spacing_rods,1);
SCM
Separating_group_spanner::set_spacing_rods (SCM smob)
{
  Grob*me = unsmob_grob (smob);
  
  for (SCM s = me->get_grob_property ("elements"); gh_pair_p (s) && gh_pair_p (ly_cdr (s)); s = ly_cdr (s))
    {
      /*
	Order of elements is reversed!
       */
      SCM elt = ly_car (s);
      Item *r = unsmob_item (elt);

      if (!r)
	continue;

      Item *rb
	= dynamic_cast<Item*> (r->find_prebroken_piece (LEFT));
      
      find_rods (r, ly_cdr (s));
      if (rb)
	find_rods (rb, ly_cdr (s));
    }

  return SCM_UNSPECIFIED ;
}

void
Separating_group_spanner::add_spacing_unit (Grob* me ,Item*i)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), i);
  me->add_dependency (i);
}





ADD_INTERFACE (Separating_group_spanner,"separation-spanner-interface",
  "Spanner that containing @code{separation-item-interface} grobs to calculate rods",
  "");
