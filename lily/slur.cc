/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998, 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  [TODO]
    * URG: share code with tie
    * begin and end should be treated as a Script.
    * damping
    * slur from notehead to stemend: c''()b''
 */

#include "slur.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "note-column.hh"
#include "stem.hh"
#include "p-col.hh"
#include "molecule.hh"
#include "debug.hh"
#include "box.hh"
#include "bezier.hh"
#include "encompass-info.hh"
#include "main.hh"

IMPLEMENT_IS_TYPE_B1(Slur,Bow);

Slur::Slur ()
{
}

void
Slur::add_column (Note_column*n)
{
  if (!n->head_l_arr_.size ())
    warning (_ ("Putting slur over rest."));
  encompass_arr_.push (n);
  add_dependency (n);
}

void
Slur::set_default_dir ()
{
  dir_ = DOWN;
  for (int i=0; i < encompass_arr_.size (); i ++) 
    {
      if (encompass_arr_[i]->dir_ < 0) 
	{
	  dir_ = UP;
	  break;
	}
    }
}

void
Slur::do_add_processing ()
{
  set_bounds (LEFT, encompass_arr_[0]);    
  if (encompass_arr_.size () > 1)
    set_bounds (RIGHT, encompass_arr_.top ());
}

void
Slur::do_pre_processing ()
{
  // don't set directions
}

void
Slur::do_substitute_dependency (Score_element*o, Score_element*n)
{
  int i;
  while ((i = encompass_arr_.find_i ((Note_column*)o->access_Item ())) >=0) 
    {
      if (n)
	encompass_arr_[i] = (Note_column*)n->access_Item ();
      else
	encompass_arr_.del (i);
    }
}

static int 
Note_column_compare (Note_column *const&n1 , Note_column* const&n2)
{
  return Item::left_right_compare (n1, n2);
}

void
Slur::do_post_processing ()
{
  encompass_arr_.sort (Note_column_compare);
  if (!dir_)
    set_default_dir ();

  Real interline_f = paper ()->interline_f ();
  Real internote_f = interline_f / 2;
  Real notewidth_f = paper ()->note_width ();
  Real slur_min = paper ()->get_var ("slur_x_minimum");

  /* 
   [OSU]: slur and tie placement

   slurs:
   * x = centre of head (upside-down: inner raakpunt stem) - d * gap

   * y = length < 5ss : horizontal raakpunt + d * 0.25 ss
     y = length >= 5ss : y next interline - d * 0.25 ss
     --> height <= 5 length ?? we use <= 3 length, now...
   */
  
  Real gap_f = paper ()->get_var ("slur_x_gap");

  Drul_array<Note_column*> extrema;
  extrema[LEFT] = encompass_arr_[0];
  extrema[RIGHT] = encompass_arr_.top ();

  Direction d=LEFT;
 
  do 
    {
      /*
        broken slur
       */
      if (extrema[d] != spanned_drul_[d]) 
	{
	  // ugh -- check if needed
	  dx_f_drul_[d] = -d 
	    *(spanned_drul_[d]->width ().length () - 0.5 * notewidth_f);

	  // prebreak
	  if (d == RIGHT)
	    {
	      dx_f_drul_[LEFT] = spanned_drul_[LEFT]->width ().length ();

	      // urg -- check if needed
	      if (encompass_arr_.size () > 1)
		dx_f_drul_[RIGHT] += notewidth_f;
	    }
	}
      /*
        normal slur
       */
      else if (extrema[d]->stem_l_ && !extrema[d]->stem_l_->transparent_b_ 
	       && extrema[d]->head_l_arr_.size ()) 
        {
	  Real notewidth_f = extrema[d]->width ().length ();
	  dy_f_drul_[d] = (int)rint (extrema[d]->stem_l_->height ()[dir_]);
	  dx_f_drul_[d] += 0.5 * notewidth_f - d * gap_f;
	  if (dir_ == extrema[d]->stem_l_->dir_)
	    {
	      if (dir_ == d)
		dx_f_drul_[d] += 0.5 * (dir_ * d) * d * notewidth_f;
	      else
		dx_f_drul_[d] += 0.25 * (dir_ * d) * d * notewidth_f;
	    }
	}
	else 
	  {
	    Real notewidth_f = extrema[d]->width ().length ();
	    dy_f_drul_[d] = (int)rint (extrema[d]->head_positions_interval ()
				       [dir_]) * internote_f;
	    dx_f_drul_[d] += 0.5 * notewidth_f - d * gap_f;
	}
	dy_f_drul_[d] += dir_ * interline_f;
	if (extrema[d]->stem_l_ && (dir_ == extrema[d]->stem_l_->dir_))
	  dy_f_drul_[d] -= dir_ * internote_f;
      }
  while (flip(&d) != LEFT);

  // now that both are set, do dependent
  do 
    {
      /*
        broken slur
       */
      if (extrema[d] != spanned_drul_[d]) 
        {
	  Direction u = d;
	  flip(&u);

	  // postbreak
	  if (d == LEFT)
	    dy_f_drul_[u] += dir_ * internote_f;

	  dy_f_drul_[d] = dy_f_drul_[(Direction)-d];

	  // pre and post
	  if (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT] < slur_min)
	    {
	      dx_f_drul_[d] -= d * slur_min 
		- (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
	      dx_f_drul_[d] = dx_f_drul_[(Direction)-d] + d * slur_min;
	    }
	}
     }
  while (flip(&d) != LEFT);

  /*
    Avoid too steep slurs.
      * slur from notehead to stemend: c''()b''
   */
  Real damp_f = paper ()->get_var ("slur_slope_damping");
  Offset d_off = Offset (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT],
    dy_f_drul_[RIGHT] - dy_f_drul_[LEFT]);
  d_off.x () += width ().length ();

  Real ratio_f = abs (d_off.y () / d_off.x ());
  if (ratio_f > damp_f)
    dy_f_drul_[(Direction)(- dir_ * sign (d_off.y ()))] -=
      dir_ * (damp_f - ratio_f) * d_off.x ();
}

Array<Offset>
Slur::get_encompass_offset_arr () const
{
  Offset left = Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]);
  left.x () += encompass_arr_[0]->stem_l_->hpos_f ();

  Offset d = Offset (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT],
    dy_f_drul_[RIGHT] - dy_f_drul_[LEFT]);
  d.x () += width ().length ();

  int first = 1;
  int last = encompass_arr_.size () - 1;

  // prebreak
  if (encompass_arr_[0] != spanned_drul_[LEFT])
    first--;

  // postbreak
  if (encompass_arr_.top () != spanned_drul_[RIGHT])
    last++;

#define RESIZE_ICE
#ifndef RESIZE_ICE

  Array<Offset> notes;
  notes.push (Offset (0,0));
//  notes.push (left);

  for (int i = first; i < last; i++)
    {
      Encompass_info info (encompass_arr_[i], dir_);
      notes.push (info.o_ - left);
//      notes.push (info.o_ - left);
    }
  notes.push (d);

#else

  int n = last - first + 2;
  Array<Offset> notes (n);
  notes[0] = Offset (0,0);
//  notes[0] = left;

  for (int i = first; i < last; i++)
    {
      Encompass_info info (encompass_arr_[i], dir_);
      notes[i - first + 1] = info.o_ - left;
//     notes[i - first + 1] = info.o_;
    }
  notes[n - 1] = Offset (d);

#endif

  return notes;
}

