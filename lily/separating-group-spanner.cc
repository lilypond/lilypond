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

static Rod
make_rod (Item *l, Item *r)
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

      Item *l = dynamic_cast<Item*> (unsmob_element (elt));
      Item *r = dynamic_cast<Item*> (unsmob_element ( next_elt));

      if (!r || !l)
	continue;
      
      Item *lb
	= dynamic_cast<Item*>(l->find_prebroken_piece (RIGHT));

      Item *rb
	= dynamic_cast<Item*>(r->find_prebroken_piece (LEFT));
      
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

  /*
    We've done our job, so we get lost. 
   */
  for (SCM s = get_elt_property ("elements"); gh_pair_p (s); s = gh_cdr (s))
    {
      Item * it =dynamic_cast<Item*>(unsmob_element (gh_car (s)));
      if (it && it->broken_b ())
	{
	  it->find_prebroken_piece (LEFT) ->suicide ();
	  it->find_prebroken_piece (RIGHT)->suicide ();
	}
      it->suicide ();
    }
  
  ((Separating_group_spanner *)this)->suicide ();
  
  return a;
}

void
Separating_group_spanner::add_spacing_unit (Score_element* me ,Item*i)
{
  Pointer_group_interface (me, "elements").add_element (i);
  me->add_dependency (i);
}


Separating_group_spanner::Separating_group_spanner (SCM s)
  : Spanner (s)  
{
  set_elt_property ("elements", SCM_EOL);
}
