/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "paper-def.hh"
#include "tie.hh"
#include "note-head.hh"
#include "p-col.hh"
#include "debug.hh"


void
Tie::set_head (Direction d, Note_head * head_l)
{
  assert (!head_l_drul_[d]);
  head_l_drul_[d] = head_l;

  add_dependency (head_l);
}

Tie::Tie()
{
  head_l_drul_[RIGHT] =0;
  head_l_drul_[LEFT] =0;
  same_pitch_b_ =false;
}

void
Tie::set_default_dir()
{
  int m= (head_l_drul_[LEFT]->position_i_ + head_l_drul_[RIGHT]->position_i_) /2;
  dir_ =  (m < 5)? DOWN : UP;	// UGH
}

void
Tie::do_add_processing()
{
  if (!(head_l_drul_[LEFT] && head_l_drul_[RIGHT]))
    warning (_("Lonely tie.. "));

  set_bounds(LEFT,head_l_drul_[LEFT]);
  set_bounds(RIGHT,head_l_drul_[RIGHT]);
}

void
Tie::do_post_processing()
{
  Real nw_f = paper()->note_width ();
  assert (head_l_drul_[LEFT] || head_l_drul_[RIGHT]);

  Direction d = LEFT;
  do
    {
      pos_i_drul_[d] =  (head_l_drul_[d])?
	head_l_drul_[d]->position_i_ : head_l_drul_[(Direction)-d]->position_i_;
    }
  while ((d *= -1) != LEFT);

  do
    {
      if (head_l_drul_[d] && head_l_drul_[d]->extremal_i_)
	{
	  pos_i_drul_[d] += 2*dir_;
	  dx_f_drul_[d] += d * 0.25;
	}
      else if (head_l_drul_[d])
	dx_f_drul_[d] += d*0.5;
      else
	{
	  pos_i_drul_[d] = pos_i_drul_[(Direction) -d];
	  dx_f_drul_[d] = -d
	    *(spanned_drul_[d]->width ().length ()/nw_f -0.5);
	}
    }
  while ((d *= -1) != LEFT);
}



void
Tie::do_substitute_dependency (Score_elem*o, Score_elem*n)
{
  Note_head *new_l =n?(Note_head*)n->item():0;
  if (o->item() == head_l_drul_[LEFT])
    head_l_drul_[LEFT] = new_l;
  else if (o->item() == head_l_drul_[RIGHT])
    head_l_drul_[RIGHT] = new_l;
}

IMPLEMENT_IS_TYPE_B1(Tie,Bow);
