/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
    warning (_ ("lonely tie"));

  Direction d = LEFT;
  Drul_array<Note_head *> new_head_drul = head_l_drul_;
  do {
    if (!head_l_drul_[d])
      new_head_drul[d] = head_l_drul_[(Direction)-d];
  } while (flip(&d) != LEFT);
  head_l_drul_ = new_head_drul;
}

void
Tie::do_post_processing()
{
  // URG: share code with slur!
  assert (head_l_drul_[LEFT] || head_l_drul_[RIGHT]);

  Real notewidth = paper ()->note_width ();
  Real interline_f = paper ()->interline_f ();
  Real tie_min = paper ()->get_var ("tie_x_minimum");

  /* 
   [OSU]: slur and tie placement

   ties:
   * x = inner raakpunt - d * gap

   * y = length < 5ss : horizontal raakpunt
     y = length >= 5ss : y next interline - d * 0.25 ss
     --> height <= 5 length ?? we use <= 3 length, now...
   */

  Real gap_f = paper ()->get_var ("slur_x_gap");

  Direction d = LEFT;
  do
    {
      dy_f_drul_[d] = .5 * interline_f * (head_l_drul_[d] 
		       ? head_l_drul_[d]->position_i_
		       : head_l_drul_[(Direction)-d]->position_i_);
    }
  while (flip(&d) != LEFT);

  do
    {
      // tie attached to outer notehead
      if (head_l_drul_[d] && head_l_drul_[d]->extremal_i_)
	{
	  if (d == LEFT)
	    dx_f_drul_[d] += notewidth;
	  dx_f_drul_[d] += -d * gap_f;
	  /* attach to outer 3/4 end of head */
	  dy_f_drul_[d] += dir_ * 0.25 * interline_f;
	}
      // tie attached to inner notehead
      else if (head_l_drul_[d] && d == LEFT)
	{
	  dx_f_drul_[d] += -d * notewidth;
	}
      // uhm? loose end of tie // tie attached to stem
      else
	{
	  dx_f_drul_[d] = -d * (spanned_drul_[d]->extent (X_AXIS).length () 
			        -0.5 * notewidth);
	}
    }
  while (flip(&d) != LEFT);

  // now that both are set, do dependent
  do
    {
      // tie attached to outer notehead
      if (!head_l_drul_[d])
	{

	/*
	 urg, this is broken
	 but who *is* going to assure that dx >= tie_min?
	 */
#if 0
	  if (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT] < tie_min)
	    {
//	      dx_f_drul_[d] -= d * tie_min 
//		- (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
	      dx_f_drul_[d] = dx_f_drul_[(Direction)-d] + d * tie_min;
	    }
#endif

	  dy_f_drul_[d] = dy_f_drul_[(Direction) -d];
	}
    }
  while (flip(&d) != LEFT);

  /*
    Avoid too steep ties
      * slur from notehead to stemend: c''()b''
   */
  Real damp_f = paper ()->get_var ("tie_slope_damping");
  Offset d_off = Offset (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT],
    dy_f_drul_[RIGHT] - dy_f_drul_[LEFT]);
  d_off.x () += extent (X_AXIS).length ();

  Real ratio_f = abs (d_off.y () / d_off.x ());
  if (ratio_f > damp_f)
    dy_f_drul_[(Direction)(- dir_ * sign (d_off.y ()))] -=
      dir_ * (damp_f - ratio_f) * d_off.x ();
}

void
Tie::do_substitute_dependency (Score_element*o, Score_element*n)
{
  Note_head *new_l =n?dynamic_cast<Note_head *> (n):0;
  if (dynamic_cast <Item *> (o) == head_l_drul_[LEFT])
    head_l_drul_[LEFT] = new_l;
  else if (dynamic_cast <Item *> (o) == head_l_drul_[RIGHT])
    head_l_drul_[RIGHT] = new_l;
}

