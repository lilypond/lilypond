/*
  item.cc -- implement Item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "p-score.hh"
#include "debug.hh"
#include "item.hh"
#include "p-col.hh"
#include "spanner.hh"

Item::Item ()
{
  unbroken_original_l_ =0;
  break_priority_i_ = 0;
  breakable_b_ = false;
  break_status_dir_ = CENTER;
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT]=0;
}

bool
Item::breakable_b () const
{
  return !unbroken_original_l_ 
    && dynamic_cast<Item*>(parent_l (X_AXIS))->breakable_b ();
}

void
Item::do_print() const
{
#ifndef NPRINT
  DOUT << "breakable_b_: " << breakable_b_ << 
    " break_status_dir_: " << break_status_dir_;
#endif
}


Real 
Item::hpos_f() const
{
  return absolute_coordinate (X_AXIS);
}

Line_of_score *
Item::line_l() const
{
  Graphical_element *g = parent_l (X_AXIS);
  if (!g)
    return 0;
  return dynamic_cast<Score_element *> (g)-> line_l ();
}

Direction
Item::break_status_dir() const
{
  return break_status_dir_;
}

void
Item::copy_breakable_items()
{
  if (broken_to_drul_[LEFT] || broken_to_drul_[RIGHT] 
      || ! breakable_b ())
    return;

  Drul_array<Item *> new_copies;
  Direction  i=LEFT;
  do 
    {
      Score_element * dolly = clone();
      Item * item_p = dynamic_cast<Item*>(dolly);

      item_p->break_status_dir_ =  i;
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
  if (visibility_lambda_)
    {
      SCM args = scm_listify (gh_int2scm (break_status_dir_), SCM_UNDEFINED);
      SCM result = gh_apply (visibility_lambda_, args);
      int trans = gh_scm2bool (gh_car (result));
      int empty = gh_scm2bool (gh_cdr (result));

      if (empty)
	set_empty (true);
      if (trans)
	transparent_b_ = true;
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

}
Item*
Item::find_prebroken_piece (Line_of_score*l) const
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
Item::find_prebroken_piece (Direction breakstatus) const
{
  if (!breakstatus)
    return (Item *) (this);	// ugh
  else
    return dynamic_cast<Item*> (broken_to_drul_[breakstatus]);
}

void
Item::handle_prebroken_dependencies()
{
  if (breakable_b_)
    Score_element::handle_prebroken_dependencies();
}

int
Item::left_right_compare(Item const *l, Item const *r)
{
  Paper_column *p1 = l->column_l ();
  Paper_column* p2 = r->column_l ();
  return p1->rank_i () - p2->rank_i ();
}


bool
Item::linked_b() const
{
  return Score_element::linked_b() || attached_span_l_arr_.size();
}

void
Item::do_junk_links()
{
  attached_span_l_arr_.set_size(0);
}

void
Item::do_unlink()
{
  Link_array<Spanner> attached=attached_span_l_arr_;
  for (int i=0; i < attached.size (); i++)
    {
      Spanner *s= attached[i];

      Direction d= LEFT;
      do {
	if (s->spanned_drul_[d] == this)
	  s->set_bounds (d, 0);
	if (unbroken_original_l_
	    && unbroken_original_l_-> broken_to_drul_[d] == this)
	  unbroken_original_l_->broken_to_drul_[d] = 0;
      } while (flip (&d) != LEFT);
    }
  assert (!attached_span_l_arr_.size ());
  unbroken_original_l_ =0;
}

Paper_column *
Item::column_l () const
{
  return dynamic_cast<Item*> (parent_l (X_AXIS))->column_l ();
}

Item::Item (Item const &s)
  : Score_element (s)
{
  unbroken_original_l_ = &s;
  /* do not copy attached_span_l_arr_ */
  breakable_b_ = s.breakable_b_;
  visibility_lambda_ = s.visibility_lambda_;
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT] =0;
  break_status_dir_ = s.break_status_dir_;
  break_priority_i_ = s.break_priority_i_;
}


void
Item::handle_prebroken_dependents ()
{
  Item * parent =  dynamic_cast<Item*> (parent_l( X_AXIS));
  if (breakable_b () && parent)
    {
       if(!(broken_to_drul_[LEFT] || broken_to_drul_[RIGHT]))
	do_break ();

      Direction d = LEFT;
      do
	{
	  broken_to_drul_[d]->dim_cache_[X_AXIS].parent_l_ =
	    &parent->broken_to_drul_[d]->dim_cache_[X_AXIS];
	  parent->broken_to_drul_[d]->add_dependency (broken_to_drul_[d]);
	}
      while ((flip (&d))!=LEFT);
    }
}

