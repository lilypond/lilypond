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
  set_bounds (d, head_l);

  add_dependency (head_l);
}

Tie::Tie()
{
  head_l_drul_[RIGHT] =0;
  head_l_drul_[LEFT] =0;
  same_pitch_b_ =false;
}


/*
  ugh: direction of the Tie is more complicated.  See [Ross] p136 and further
 */
void
Tie::set_default_dir()
{
  int m= (head_l_drul_[LEFT]->position_i_ 
	  + head_l_drul_[RIGHT]->position_i_) /2;
  dir_ =  (m < 0)? DOWN : UP;
}

void
Tie::do_add_processing()
{
  if (!(head_l_drul_[LEFT] && head_l_drul_[RIGHT]))
    warning (_("Lonely tie.. "));

  Direction d = LEFT;
  Drul_array<Note_head *> new_head_drul = head_l_drul_;
  do {
    if (!head_l_drul_[d])
      new_head_drul[d] = head_l_drul_[(Direction)-d];
  } while ((d *= -1) != LEFT);
  head_l_drul_ = new_head_drul;
}

void
Tie::do_post_processing()
{
  Real nw_f = paper ()->note_width ();
  Real interline_f = paper ()->interline_f ();
  assert (head_l_drul_[LEFT] || head_l_drul_[RIGHT]);

  /* 
   [OSU]: slur and tie placement

   ties:
   * x = inner raakpunt - d * gap

   * y = length < 5ss : horizontal raakpunt
     y = length >= 5ss : y next interline - d * 0.25 ss
     --> height <= 5 length ?? we use <= 3 length, now...

   * suggested gap = ss / 5;
   */
  // jcn: 1/5 seems so small?
  Real gap_f = interline_f / 2; // 5;

  Direction d = LEFT;
  do
    {
      dy_f_drul_[d] = .5 * interline_f * (head_l_drul_[d] 
		       ? head_l_drul_[d]->position_i_
		       : head_l_drul_[(Direction)-d]->position_i_);
    }
  while ((d *= -1) != LEFT);

  do
    {
      if (head_l_drul_[d] && head_l_drul_[d]->extremal_i_)
	{
	  /* normal tie between noteheads, with gap of space */
	  dx_f_drul_[d] += -d * (0.5 * nw_f + gap_f);
	  /* attach to outer 3/4 end of head */
	  dy_f_drul_[d] += dir_ * 0.25 * interline_f;
	}
      else if (head_l_drul_[d])
	{
	  dx_f_drul_[d] += d*0.5 * nw_f;
	}
      else
	{
	  dy_f_drul_[d] = dy_f_drul_[(Direction) -d];
	  dx_f_drul_[d] = -d * (spanned_drul_[d]->width ().length () 
			        -0.5 * nw_f);
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
