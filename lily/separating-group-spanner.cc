/*   
  separating-group-spanner.cc --  implement Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "separation-item.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "dimensions.hh"
#include "group-interface.hh"

static void
do_rod (Item *l, Item *r)
{
  Rod rod;

  Interval li (Separation_item::my_width (l));
  Interval ri (Separation_item::my_width (r));

  rod.item_l_drul_[LEFT] =l;
  rod.item_l_drul_[RIGHT]=r;

  if (li.empty_b () || ri.empty_b ())
    rod.distance_f_ = 0;
  else
    rod.distance_f_ = li[RIGHT] - ri[LEFT];

  rod.columnize ();
  rod.add_to_cols ();
}
  
MAKE_SCHEME_CALLBACK(Separating_group_spanner,set_spacing_rods);
SCM
Separating_group_spanner::set_spacing_rods (SCM smob)
{
  Score_element*me = unsmob_element (smob);
  
  for (SCM s = me->get_elt_property ("elements"); gh_pair_p (s) && gh_pair_p (gh_cdr (s)); s = gh_cdr (s))
    {
      /*
	Order of elements is reversed!
       */
      SCM elt = gh_cadr (s);
      SCM next_elt = gh_car (s);

      Item *l = dynamic_cast<Item*> (unsmob_element (elt));
      Item *r = dynamic_cast<Item*> (unsmob_element ( next_elt));

      if (!r || !l)
	continue;
      
      Item *lb
	= dynamic_cast<Item*>(l->find_prebroken_piece (RIGHT));

      Item *rb
	= dynamic_cast<Item*>(r->find_prebroken_piece (LEFT));
      
      do_rod(l,  r);
      if (lb)
	{
	  do_rod (lb, r);
	}
      
      if (rb)
	{
	  do_rod (l, rb);
	}
      
      if (lb && rb)
	{
	  do_rod (lb, rb);

	}
    }

  /*
    We've done our job, so we get lost. 
   */
  for (SCM s = me->get_elt_property ("elements"); gh_pair_p (s); s = gh_cdr (s))
    {
      Item * it =dynamic_cast<Item*>(unsmob_element (gh_car (s)));
      if (it && it->broken_b ())
	{
	  it->find_prebroken_piece (LEFT) ->suicide ();
	  it->find_prebroken_piece (RIGHT)->suicide ();
	}
      it->suicide ();
    }
  me->suicide ();
  return SCM_UNSPECIFIED ;
}

void
Separating_group_spanner::add_spacing_unit (Score_element* me ,Item*i)
{
  Pointer_group_interface (me, "elements").add_element (i);
  me->add_dependency (i);
}


void
Separating_group_spanner::set_interface (Score_element*me)
{
  me->set_elt_property ("elements", SCM_EOL);
}
