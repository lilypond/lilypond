/*   
  separating-group-spanner.cc --  implement Separating_group_spanner
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separating-group-spanner.hh"
#include "single-malt-grouping-item.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "dimensions.hh"

static Rod
make_rod (Single_malt_grouping_item *l, Single_malt_grouping_item *r)
{
  Rod rod;
  rod.item_l_drul_[LEFT] =l;
  rod.item_l_drul_[RIGHT]=r;

  Interval li (l->my_width ());
  Interval ri (r->my_width ());
  
  if (li.empty_b () || ri.empty_b ())
    rod.distance_f_ = 0;
  else
    rod.distance_f_ = li[RIGHT] - ri[LEFT];

  return rod;
}
  

Array<Rod>
Separating_group_spanner::get_rods () const
{
  Array<Rod> a;
  
  for (SCM s = get_elt_property ("elements"); gh_pair_p (s) && gh_pair_p (gh_cdr (s)); s = gh_cdr (s))
    {
      /*
	Order of elements is reversed!
       */
      SCM elt = gh_cadr (s);
      SCM next_elt = gh_car (s);

      Single_malt_grouping_item *l = dynamic_cast<Single_malt_grouping_item*> (unsmob_element (elt));
      Single_malt_grouping_item *r = dynamic_cast<Single_malt_grouping_item*> (unsmob_element ( next_elt));

      if (!r || !l)
	continue;
      
      Single_malt_grouping_item *lb
	= dynamic_cast<Single_malt_grouping_item*>(l->find_broken_piece (RIGHT));

      Single_malt_grouping_item *rb
	= dynamic_cast<Single_malt_grouping_item*>(r->find_broken_piece (LEFT));
      
      a.push (make_rod(l,  r));
      if (lb)
	{
	  Rod rod(make_rod (lb, r));
	  a.push (rod);
	}
      
      if (rb)
	{
	  a.push (make_rod (l, rb));
	}
      
      if (lb && rb)
	{
	  Rod rod(make_rod (lb, rb));
	  a.push (rod);
	}
    }
      
  return a;
}

void
Separating_group_spanner::add_spacing_unit (Single_malt_grouping_item*i)
{
  set_elt_property ("elements",
		    gh_cons (i->self_scm_,
			     get_elt_property ("elements")));
  add_dependency (i);
}


Separating_group_spanner::Separating_group_spanner ()
{
  set_elt_property ("elements", SCM_EOL);
}
