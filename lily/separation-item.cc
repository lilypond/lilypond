/*   
     separation-item.cc --  implement Separation_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separation-item.hh"
#include "paper-column.hh"
#include "debug.hh"
#include "group-interface.hh"

bool
Separation_item::has_interface (Grob *g)
{
  return g->has_interface (ly_symbol2scm ("separation-item-interface"));
}

void
Separation_item::add_item (Grob*s,Item* i)
{
  assert (i);
  Pointer_group_interface::add_grob (s, ly_symbol2scm ("elements"),i);
  s->add_dependency (i);
}

/*
  DOCME:

  why don't we use extent()
 */
Interval
Separation_item::my_width (Grob *me)
{
  Item *item = dynamic_cast<Item*> (me);
  Paper_column * pc = item->column_l ();
  Interval w;
  
  for (SCM s =  me->get_grob_property ("elements"); gh_pair_p (s); s = ly_cdr (s))
    {
      SCM elt = ly_car (s);
      if (!unsmob_grob (elt))
	continue;

      Item *il = unsmob_item (elt);
      if (pc != il->column_l ())
	{
	  /* this shouldn't happen, but let's continue anyway. */
	  programming_error (_ ("Separation_item:  I've been drinking too much"));
	  continue;		/*UGH UGH*/ 
	}

      if (to_boolean (il->get_grob_property ("no-spacing-rods")))
	{
	  continue;
	}

      Interval iv (il->extent (pc, X_AXIS));
      if (!iv.empty_b ())
	{
	  w.unite (iv);
	}
    }

  SCM pad = me->get_grob_property ("padding");

  if (gh_number_p (pad))
  {
    w[RIGHT] += gh_scm2double (pad)/2;
    w[LEFT] -= gh_scm2double (pad)/2;    
  }
  
  return w;
 // add this->offset_ ? this-> relative_coordinate ()? 
}





ADD_INTERFACE (Separation_item,"separation-item-interface",
  "Item that computes widths to generate spacing rods.

Calc dimensions for the Separating_group_spanner; this has to be
an item to get dependencies correct.  It can't be an grob_group
since these usually are in a different X_group
",
  "elements");
