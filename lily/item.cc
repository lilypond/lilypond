/*
  item.cc -- implement Item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension-cache.hh"
#include "paper-score.hh"
#include "debug.hh"
#include "item.hh"
#include "paper-column.hh"
#include "spanner.hh"
#include "lily-guile.hh"
#include "line-of-score.hh"

Item::Item (SCM s)
  : Score_element (s)
{
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT]=0;
}

/**
   Item copy ctor.  Copy nothing: everything should be a elt property
   or a special purpose pointer (such as broken_to_drul_[]) */
Item::Item (Item const &s)
  : Score_element (s)
{
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT] =0;
}



bool
Item::breakable_b () const
{
  if (original_l_ )
    return false;
  
  Item * i  =dynamic_cast<Item*> (parent_l (X_AXIS));
  return (i) ?  i->breakable_b () : to_boolean (get_elt_property ("breakable"));
}

Line_of_score *
Item::line_l() const
{
  Score_element *g = parent_l (X_AXIS);
  return g ?  g->line_l () : 0;
}


void
Item::copy_breakable_items()
{
  Drul_array<Item *> new_copies;
  Direction  i=LEFT;
  do 
    {
      Score_element * dolly = clone();
      Item * item_p = dynamic_cast<Item*>(dolly);
      pscore_l_->line_l_->typeset_element (item_p);
      new_copies[i] =item_p;
    }
  while (flip(&i) != LEFT);
  broken_to_drul_= new_copies;
}


bool
Item::broken_b () const
{
  return broken_to_drul_[LEFT] || broken_to_drul_[RIGHT];
}


/*
  Generate items for begin and end-of line.
 */
void
Item::discretionary_processing()
{
  if (broken_b ())
    return;

  if (breakable_b ())
    copy_breakable_items();
}

Score_element*
Item::find_broken_piece (Line_of_score*l) const
{
  if (line_l() == l) 
    return (Item*)(this);

  Direction d = LEFT;
  do {
    Score_element *s = broken_to_drul_[d];
    if (s && s->line_l () == l)
      return s;
  }
  while (flip (&d) != LEFT);

  return 0;
}


Item*
Item::find_prebroken_piece (Direction d) const
{
  Item * me = (Item *) (this);	
  if (!d)
    return me;
  return dynamic_cast<Item*> (broken_to_drul_[d]);
}

Paper_column *
Item::column_l () const
{
  return dynamic_cast<Item*> (parent_l (X_AXIS))->column_l ();
}

Direction
Item::break_status_dir () const
{
  if (original_l_)
    {
      Item * i = dynamic_cast<Item*> (original_l_);

      return (i->broken_to_drul_[LEFT] == this) ? LEFT : RIGHT;
    }
  else
    return CENTER;
}

void
Item::handle_prebroken_dependencies ()
{
  if (original_l_)
    {
      pointer_alist_
	= handle_broken_smobs (original_l_->pointer_alist_,
			       gh_int2scm (break_status_dir ()));
    }
  
  /*
    Can't do this earlier, because try_visibility_lambda () might set
    the elt property transparent, which would then be copied.
  */
  SCM vis = get_elt_property ("visibility-lambda");
  if (gh_procedure_p (vis))
    {
      SCM args = scm_listify (gh_int2scm (break_status_dir ()), SCM_UNDEFINED);
      SCM result = gh_apply (vis, args);
      bool trans = gh_scm2bool (gh_car (result));
      bool empty = gh_scm2bool (gh_cdr (result));
      
      if (empty && trans)
	suicide ();
      else if (empty)
	{
	  set_extent_callback (0, X_AXIS);
	  set_extent_callback (0,  Y_AXIS);
	}
      else if (trans)
	set_elt_property ("molecule-callback", SCM_BOOL_T);
    }
}

