/*   
     separation-item.cc --  implement Separation_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "separation-item.hh"
#include "paper-column.hh"
#include "warn.hh"
#include "group-interface.hh"
#include "accidental-placement.hh"

void
Separation_item::add_item (Grob*s,Item* i)
{
  assert (i);
  Pointer_group_interface::add_grob (s, ly_symbol2scm ("elements"),i);
  s->add_dependency (i);
}

void
Separation_item::add_conditional_item (Grob* me , Grob *e)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("conditional-elements"), e);
}

Interval
Separation_item::conditional_width (Grob * me, Grob * left)
{
  Interval w = width (me);

  Item *item = dynamic_cast<Item*> (me);
  Paper_column * pc = item->column_l ();
  
  
  for (SCM s =  me->get_grob_property ("conditional-elements"); gh_pair_p (s); s = ly_cdr (s))
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

      if (Accidental_placement::has_interface (il))
	{
	  w.unite (Accidental_placement::get_relevant_accidental_extent (il, pc, left));
	}
    }

  SCM pad = me->get_grob_property ("padding");

  if (gh_number_p (pad))
    {
      w[RIGHT] += gh_scm2double (pad)/2;
      w[LEFT] -= gh_scm2double (pad)/2;    
    }
  return w;
}

Interval
Separation_item::width (Grob *me)
{
  SCM sw = me->get_grob_property ("extent-X");
  if (ly_number_pair_p (sw))
    {
      return ly_scm2interval (sw);
    }

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


  me->set_grob_property ("extent-X", ly_interval2scm (w));
  
  
  return w;
 // add this->offset_ ? this-> relative_coordinate ()? 
}





ADD_INTERFACE (Separation_item,"separation-item-interface",
  "Item that computes widths to generate spacing rods.

Calc dimensions for the Separating_group_spanner; this has to be
an item to get dependencies correct.  It can't be an grob_group
since these usually are in a different X_group
",
  "extent-X conditional-elements elements");
