/*
  item.cc -- implement Item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

bool
Item::breakable_b () const
{
  if (original_l_ )
    return false;
  
  Item * i  =dynamic_cast<Item*> (parent_l (X_AXIS));
  return (i) ?  i->breakable_b () : get_elt_property( breakable_scm_sym) != SCM_BOOL_F;
}

void
Item::do_print() const
{
}


Real 
Item::hpos_f() const
{
  return relative_coordinate (0, X_AXIS);
}

Line_of_score *
Item::line_l() const
{
  Graphical_element *g = parent_l (X_AXIS);
  if (!g)
    return 0;
  return dynamic_cast<Score_element *> (g)-> line_l ();
}


void
Item::copy_breakable_items()
{
  if (broken_to_drul_[LEFT] || broken_to_drul_[RIGHT]
      || !breakable_b ())
    return ;

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
  try_visibility_lambda ();
}

void
Item::try_visibility_lambda ()
{
  SCM vis = remove_elt_property (visibility_lambda_scm_sym);
  if (vis != SCM_BOOL_F)
    {
      SCM args = scm_listify (gh_int2scm (break_status_dir ()), SCM_UNDEFINED);
      SCM result = gh_apply ( SCM_CDR(vis), args);
      int trans = gh_scm2bool (gh_car (result));
      int empty = gh_scm2bool (gh_cdr (result));

      if (empty)
	set_empty (true, X_AXIS, Y_AXIS);
      if (trans)
	set_elt_property (transparent_scm_sym, SCM_BOOL_T);
    }
}

void
Item::do_break ()
{
  copy_breakable_items();
  handle_prebroken_dependencies();
  
  /*
    Otherwise the broken items won't be pre_process()'ed.
  */
  add_dependency (broken_to_drul_[LEFT]);
  add_dependency (broken_to_drul_[RIGHT]);
}

void
Item::do_breakable_col_processing()
{
  if (breakable_b ())
    do_break ();
  else
    try_visibility_lambda ();
}

Score_element*
Item::find_broken_piece (Line_of_score*l) const
{
  if (line_l() == l) 
    return (Item*)(this);
  else if (broken_to_drul_[LEFT] && broken_to_drul_[LEFT]->line_l() == l)
    return broken_to_drul_[LEFT];
  else if (broken_to_drul_[RIGHT] && broken_to_drul_[RIGHT]->line_l() == l)
    return broken_to_drul_[RIGHT];

  return 0;
}

Item*
Item::find_broken_piece (Direction d) const
{
  if (!d)
    return (Item *) (this);	// ugh
  else
    return dynamic_cast<Item*> (broken_to_drul_[d]);
}

void
Item::handle_prebroken_dependencies()
{
  if (original_l_)
    Score_element::handle_prebroken_dependencies();
}

bool
Item::broken_original_b () const
{
  return broken_to_drul_[LEFT] || broken_to_drul_[RIGHT];
}

int
Item::left_right_compare(Item const *l, Item const *r)
{
  Paper_column *p1 = l->column_l ();
  Paper_column* p2 = r->column_l ();
  return p1->rank_i () - p2->rank_i ();
}

Paper_column *
Item::column_l () const
{
  return dynamic_cast<Item*> (parent_l (X_AXIS))->column_l ();
}

Item::Item (Item const &s)
  : Score_element (s)
{
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT] =0;
}


void
Item::handle_prebroken_dependents ()
{
  Item * parent =  dynamic_cast<Item*> (parent_l (X_AXIS));
  if (breakable_b () && parent)
    {
       if(!(broken_to_drul_[LEFT] || broken_to_drul_[RIGHT]))
	do_break ();

      Direction d = LEFT;
      do
	{
	  Item * broken_self = find_broken_piece (d);
	  Item * broken_parent = parent->find_broken_piece (d);

	  broken_self->set_parent (broken_parent, X_AXIS);

	  /*
	    ugh. Should do this is after breaking?
	   */
	  if (!broken_self->parent_l (Y_AXIS))
	    {
	      Score_element * yparent =dynamic_cast<Score_element*>(parent_l (Y_AXIS));
	      Item *yparenti = dynamic_cast<Item*> (yparent);
	      Item *broken_yparent = yparenti ?
		yparenti->find_broken_piece (d) : 0;
	      
	      if (!yparent)
		programming_error ("Vertical refpoint lost!");
	      else if (yparenti)
		{
		  broken_self->set_parent (broken_yparent, Y_AXIS);
		}
	    }
	}
      while ((flip (&d))!=LEFT);
    }
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

