/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998, 1998 Han-Wen Nienhuys <hanwen@stack.nl>
    Jan Nieuwenhuizen <jan@digicash.com>
*/

/*
  [TODO]
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
#include "boxes.hh"
#include "bezier.hh"
#include "encompass-info.hh"
// #include "main.hh"

IMPLEMENT_IS_TYPE_B1(Slur,Bow);

Slur::Slur ()
{
}

void
Slur::add (Note_column*n)
{
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
Slur::do_substitute_dependency (Score_elem*o, Score_elem*n)
{
  int i;
  while ((i = encompass_arr_.find_i ((Note_column*)o->item ())) >=0) 
    {
      if (n)
	encompass_arr_[i] = (Note_column*)n->item ();
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

  Real interline = paper ()->interline_f ();
  Real internote = interline / 2;
  Real notewidth = paper ()->note_width ();
  Real const SLUR_MIN = 2.0 * interline;

  /* 
   [OSU]: slur and tie placement

   slurs:
   * x = centre of head (upside-down: inner raakpunt stem) - d * gap

   * y = length < 5ss : horizontal raakpunt + d * 0.25 ss
     y = length >= 5ss : y next interline - d * 0.25 ss
     --> height <= 5 length ?? we use <= 3 length, now...

   * suggested gap = ss / 5;
   */
  // jcn: 1/5 seems so small?
  Real gap_f = interline / 2; // 5;
  
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
	  dx_f_drul_[d] = -d 
	    *(spanned_drul_[d]->width ().length () -0.5*notewidth);
	  Direction u = d;
	  flip(&u);
	  if ((extrema[u] == spanned_drul_[u]) && extrema[u]->stem_l_)
	    {
	      dy_f_drul_[d] = extrema[u]->stem_l_->height ()[dir_];
	      dy_f_drul_[u] = extrema[u]->stem_l_->height ()[dir_];
	    }

	  // prebreak
	  if (d == RIGHT)
	    {
	      dx_f_drul_[LEFT] = spanned_drul_[LEFT]->width ().length ();
//	      dx_f_drul_[LEFT] -= 2 * notewidth;

	      // urg
	      if (encompass_arr_.size () > 1)
		dx_f_drul_[RIGHT] += notewidth;
	    }

	  // postbreak
	  if (d == LEFT)
	    dy_f_drul_[d] += 2.0 * dir_ * internote;
	}
      /*
        normal slur
       */
      else if (extrema[d]->stem_l_ && !extrema[d]->stem_l_->transparent_b_) 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->stem_l_->height ()[dir_]);
	  dx_f_drul_[d] += 0.5 * notewidth - d * gap_f;
	  if (dir_ == extrema[d]->stem_l_->dir_)
	    {
	      if (dir_ == d)
		dx_f_drul_[d] += 0.5 * (dir_ * d) * d * notewidth;
	      else
		dx_f_drul_[d] += 0.25 * (dir_ * d) * d * notewidth;
	    }
	}
      else 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->head_positions_interval ()
	    [dir_])* internote;
	}
      dy_f_drul_[d] += dir_ * interline;
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
	  // pre and post
	  if (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT] < SLUR_MIN)
	    {
	      dx_f_drul_[d] -= d * SLUR_MIN 
		- (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
	      dx_f_drul_[d] = dx_f_drul_[(Direction)-d] + d * SLUR_MIN;
	    }
	  dy_f_drul_[d] = dy_f_drul_[(Direction)-d];
	}
     }
  while (flip(&d) != LEFT);
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

  for (int i = first; i < last; i++)
    {
      Encompass_info info (encompass_arr_[i], dir_);
      notes.push (info.o_ - left);
    }
  notes.push (d);

#else

  int n = last - first + 2;
  Array<Offset> notes (n);
  notes[0] = Offset (0,0);

  for (int i = first; i < last; i++)
    {
      Encompass_info info (encompass_arr_[i], dir_);
      notes[i - first + 1] = info.o_ - left;
    }
  notes[n - 1] = Offset (d);

#endif

  return notes;
}

