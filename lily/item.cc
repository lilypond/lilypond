/*
  item.cc -- implement Item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "p-score.hh"
#include "debug.hh"
#include "item.hh"
#include "p-col.hh"
// #include "elem-group.hh"
#include "spanner.hh"

Item::Item()
{
  break_priority_i_ = 0;
  breakable_b_ = false;
  break_status_i_ = CENTER;
  broken_to_drul_[LEFT] = broken_to_drul_[RIGHT]=0;
}

IMPLEMENT_IS_TYPE_B1(Item, Score_elem);

void
Item::do_print() const
{
#ifndef NPRINT
  DOUT << "breakable_b_: " << breakable_b_ << 
    " break_status_i_: " <<break_status_i_;
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
  return  (axis_group_l_a_[X_AXIS])? axis_group_l_a_[X_AXIS]->line_l() : 0;
}

int
Item::break_status_i() const
{
  return break_status_i_;
}

void
Item::copy_breakable_items()
{
  if (broken_to_drul_[LEFT] || broken_to_drul_[RIGHT])
    return;
  Drul_array<Item *> new_copies;
  Direction  i=LEFT;
  do 
    {
      Item * item_p = clone()->item ();

      item_p->break_status_i_ =  i;
      pscore_l_->typeset_element (item_p);
      item_p->handle_prebroken_dependencies();
      new_copies[i] =item_p;
    }
  while (flip(&i) != LEFT);
  broken_to_drul_= new_copies;
}

void
Item::do_breakable_col_processing()
{
  if (!breakable_b_)
    return;

  if (!column_l ()->breakable_b_)
    return;

  copy_breakable_items();
  handle_prebroken_dependencies();

  /*
    Otherwise the broken items won't be pre_process()'ed.
    */
  add_dependency (broken_to_drul_[LEFT]);
  add_dependency (broken_to_drul_[RIGHT]);    
}

Item*
Item::find_prebroken_piece (Line_of_score*l) const
{
  if (line_l() == l) 
    return (Item*)this;
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
    return (Item *) this;	// ugh
  else
    return (Item*) broken_to_drul_[breakstatus];
}

void
Item::handle_prebroken_dependencies()
{
  if (breakable_b_)
    Score_elem::handle_prebroken_dependencies();
}

int
Item::left_right_compare(Item const *l, Item const *r)
{
  while (!l->is_type_b (Paper_column::static_name ()))
    l = l->axis_group_l_a_[X_AXIS]->item();
  while (!r->is_type_b (Paper_column::static_name ()))
    r = r->axis_group_l_a_[X_AXIS]->item();

  Paper_column *p1 = (Paper_column*)l;
  Paper_column* p2 = (Paper_column*)r;
  return p1->rank_i () - p2->rank_i ();
}


bool
Item::linked_b() const
{
  return Score_elem::linked_b() || attached_span_l_arr_.size();
}

void
Item::do_junk_links()
{
  attached_span_l_arr_.set_size(0);
}

void
Item::do_unlink()
{
  Link_array<Spanner> attached = attached_span_l_arr_;

  for (int i=0; i < attached_span_l_arr_.size (); i++) {
    Spanner *&s= attached_span_l_arr_[i];
    if (s->spanned_drul_[LEFT] == this)
      s->set_bounds (LEFT, 0);
    if  (s->spanned_drul_[RIGHT] == this)
      s->set_bounds (RIGHT,0);
    s =0;
  }
  attached_span_l_arr_.set_size (0);
}

Paper_column *
Item::column_l () const
{
  return axis_group_l_a_[X_AXIS]->item ()->column_l ();
}
