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

Item::Item ()
{
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT]=0;
}

/**
   Item copy ctor.  Copy nothing: everything should be a elt property
   or a special purpose poitner (such as broken_to_drul_[]) */
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
  return (i) ?  i->breakable_b () : to_boolean (get_elt_property( "breakable"));
}

Line_of_score *
Item::line_l() const
{
  Score_element *g = parent_l (X_AXIS);
  if (!g)
    return 0;
  return dynamic_cast<Score_element *> (g)-> line_l ();
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
      pscore_l_->typeset_element (item_p);
      new_copies[i] =item_p;
    }
  while (flip(&i) != LEFT);
  broken_to_drul_= new_copies;

  do 
    {
       broken_to_drul_[i]->handle_prebroken_dependencies();
       broken_to_drul_[i]->try_visibility_lambda();
    }
  while (flip(&i) != LEFT);
}

void
Item::try_visibility_lambda ()
{
  SCM vis = remove_elt_property ("visibility-lambda");
  if (gh_procedure_p (vis))
    {
      SCM args = scm_listify (gh_int2scm (break_status_dir ()), SCM_UNDEFINED);
      SCM result = gh_apply (vis, args);
      bool trans = gh_scm2bool (gh_car (result));
      bool empty = gh_scm2bool (gh_cdr (result));

      if (empty)
	{
	  set_empty (X_AXIS);
	  set_empty ( Y_AXIS);
	}
      if (trans)
	set_elt_property ("transparent", SCM_BOOL_T);
    }
}

bool
Item::broken_b () const
{
  return broken_to_drul_[LEFT] || broken_to_drul_[RIGHT];
}

void
Item::do_break ()
{
  if (broken_b ())
    return;

  if (breakable_b ())
    {
      copy_breakable_items();
      handle_prebroken_dependencies();
  
      /*
    Otherwise the broken items won't be pre_process()'ed.
  */
  
      if (broken_to_drul_[LEFT])
	{
	  add_dependency (broken_to_drul_[LEFT]);
	  add_dependency (broken_to_drul_[RIGHT]);
	}
    }
  try_visibility_lambda ();	// ugh.
}

void
Item::do_breakable_col_processing()
{
  do_break ();
}

Score_element*
Item::find_broken_piece (Line_of_score*l) const
{
  if (line_l() == l) 
    return (Item*)(this);

  Direction d = LEFT;
  do {
    Score_element *s = find_broken_piece (d);
    if (s && s->line_l () == l)
      return s;
  }
  while (flip (&d) != LEFT);

  return 0;
}

Item*
Item::find_broken_piece (Direction d) const
{
  Item * me = (Item *) (this);	
  if (!d)
    return me;
  else if (breakable_b ())
    {
      me->do_break ();
      return dynamic_cast<Item*> (broken_to_drul_[d]);
    }
  else
    return 0;
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


