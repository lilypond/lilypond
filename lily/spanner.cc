/*
  spanner.cc -- implement Spanner

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "spanner.hh"
#include "p-col.hh"
#include "p-score.hh"
#include "tex-outputter.hh"
#include "molecule.hh"

IMPLEMENT_IS_TYPE_B1(Spanner,Score_element);

void
Spanner::do_print() const
{
#ifndef NPRINT
  DOUT << "Between " << spanned_drul_[LEFT]->name ()
       << " and " << spanned_drul_[RIGHT]->name() << '\n';
  if (broken_into_l_arr_.size())
    {
      DOUT << "with broken pieces:\n";
      for (int i=0; i < broken_into_l_arr_.size (); i++)
	broken_into_l_arr_[i]->print ();
    }  
#endif
}

void
Spanner::break_into_pieces ()
{
  if (broken_into_l_arr_.size())
    return; 
	 
  Item * left = spanned_drul_[LEFT];
  Item * right = spanned_drul_[RIGHT];
  
  if  (left == right)
    {
      warning (_ ("left spanpoint is right spanpoint\n"));
      return;
    }
  
  Link_array<Item> break_cols = pscore_l_->broken_col_range (left,right);
  Link_array<Spanner> broken_into_l_arr;

  break_cols.insert (left,0);
  break_cols.push (right);

  for (int i=1; i < break_cols.size(); i++) 
    {
      Spanner* span_p = clone()->access_Spanner ();
      left = break_cols[i-1];
      right = break_cols[i];
      if (!right->line_l())
	right = right->find_prebroken_piece(LEFT);
      if (!left->line_l())
	left = left->find_prebroken_piece(RIGHT);

      assert (left&&right && left->line_l() == right->line_l());

      span_p->set_bounds(LEFT,left);
      span_p->set_bounds(RIGHT,right);
	
      pscore_l_->typeset_broken_spanner (span_p);
      broken_into_l_arr.push (span_p);
    }
   
  broken_into_l_arr_ = broken_into_l_arr;
}

void
Spanner::set_my_columns()
{
  Direction i = (Direction)1;
  do 
    {
      if (!spanned_drul_[i]->line_l())
	set_bounds(i,spanned_drul_[i]->find_prebroken_piece((Direction)-i));
    } 
  while (flip(&i) != 1);
}       


void
Spanner::set_bounds(Direction d, Item*i)
{
  if (spanned_drul_[d])
    spanned_drul_[d]->attached_span_l_arr_.substitute(this,0);
  
  spanned_drul_[d] =i;
  if (i)
    i->attached_span_l_arr_.push(this);

  if  (spanned_drul_[Direction(-d)] == spanned_drul_[d]
       && i)
    warning (_f ("Spanner `%s\' with equal left and right spanpoints", name ()));
}

void
Spanner::do_break_processing()
{
  if (!line_l())
    {
      break_into_pieces ();
      for (int i=0; i < broken_into_l_arr_.size(); i++)
	broken_into_l_arr_[i]->handle_broken_dependencies();
    }
  else 
    {
      handle_broken_dependencies();
    }
}

Spanner* 
Spanner::access_Spanner ()
{
  return this;
}

Spanner::Spanner ()
{
  spanned_drul_[LEFT]=0;
  spanned_drul_[RIGHT]=0;
}

void
Spanner::do_brew_molecule () 
{
  if (transparent_b_)
    return ;
  Molecule *output= brew_molecule_p ();
  Offset left_off (spanned_drul_[LEFT]->absolute_coordinate(X_AXIS), 0);
  Offset o = absolute_offset() + left_off;
  pscore_l_->outputter_l_->output_molecule (output, o, name ());
  delete output;
}

Interval
Spanner::do_width() const
{
  Real l = spanned_drul_[LEFT]->absolute_coordinate (X_AXIS);
  Real r = spanned_drul_[RIGHT]->absolute_coordinate (X_AXIS);
  assert (r>=l);
	
  return Interval (0, r-l);
}

Line_of_score *
Spanner::line_l() const
{
  if (!spanned_drul_[LEFT] || !spanned_drul_[RIGHT])
    return 0;
  if (spanned_drul_[LEFT]->line_l() != spanned_drul_[RIGHT]->line_l())
    return 0;
  return spanned_drul_[LEFT]->line_l();
}


Spanner*
Spanner::find_broken_piece (Line_of_score*l) const
{
  for (int i=0; i < broken_into_l_arr_.size(); i++)
    if (broken_into_l_arr_[i]->line_l() == l)
      return broken_into_l_arr_[i];
  return 0;				   
	  
}

bool
Spanner::broken_b() const
{
  return broken_into_l_arr_.size();
}

void
Spanner::do_unlink() 
{
  set_bounds (LEFT, 0);
  set_bounds (RIGHT, 0);
}

void
Spanner::do_junk_links()
{
  spanned_drul_[LEFT] = spanned_drul_[RIGHT] =0;
}

Array<Rod>
Spanner::get_rods () const
{
  Array<Rod> r;
  return r;
}

void
Spanner::do_space_processing ()
{
  Array<Rod> rs (get_rods ());
  for (int i=0; i < rs.size (); i++)
    {
      rs[i].add_to_cols ();
    }
}
