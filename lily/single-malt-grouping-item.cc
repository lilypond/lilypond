/*   
  single-malt-grouping-item.cc --  implement Single_malt_grouping_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "single-malt-grouping-item.hh"
#include "paper-column.hh"
#include "debug.hh"

Single_malt_grouping_item ::Single_malt_grouping_item()
{
  set_elt_property ("transparent", SCM_BOOL_T);
  set_elt_property ("elements", SCM_EOL);

  // this is weird! , but needed!
  set_extent_callback (0, X_AXIS);
  set_extent_callback (0,  Y_AXIS);

}

void
Single_malt_grouping_item::add_item (Item* i)
{
  assert (i);
  set_elt_property ("elements",
		    gh_cons (i->self_scm_,
			     get_elt_property ("elements")));

  add_dependency (i);
}

Interval
Single_malt_grouping_item::my_width () const
{
  Paper_column * pc = column_l ();
  Interval w;
  
  for (SCM s = get_elt_property ("elements"); gh_pair_p (s); s = gh_cdr (s))
    {
      SCM elt = gh_car (s);
      if (!SMOB_IS_TYPE_B(Score_element, elt))
	continue;

      
      
      Item *il = dynamic_cast<Item*> (SMOB_TO_TYPE (Score_element, elt));
      if (pc != il->column_l ())
	{
	  /* this shouldn't happen, but let's continue anyway. */
	  programming_error (_("Single_malt_grouping_item:  I've been drinking too much"));
	  continue;		/*UGH UGH*/ 
	}

      if (to_boolean (il->get_elt_property ("no-spacing-rods")))
	{
	  continue;
	}

      Interval iv (il->extent (X_AXIS));
      if (!iv.empty_b ())
	{
	  Real off = il->relative_coordinate (pc, X_AXIS);
	  w.unite  (iv + off);
	}
    }

  return w;
 // add this->offset_ ? this-> relative_coordinate ()? 
}



