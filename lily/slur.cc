/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999, 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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


Slur::Slur ()
{
}

void
Slur::add_column (Note_column*n)
{
  if (!n->head_l_arr_.size ())
    warning (_ ("Putting slur over rest."));
  encompass_arr_.push (n);
  //  n->stem_l_->slur_l_ = this;
  add_dependency (n);
}

Direction
Slur::get_default_dir () const
{
  Direction d = DOWN;
  for (int i=0; i < encompass_arr_.size (); i ++) 
    {
      if (encompass_arr_[i]->dir () < 0) 
	{
	  d = UP;
	  break;
	}
    }
  return d;
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
Slur::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  int i;
  while ((i = encompass_arr_.find_i (dynamic_cast<Note_column *> (o))) >=0) 
    {
      if (n)
	encompass_arr_[i] = dynamic_cast<Note_column *> (n);
      else
	encompass_arr_.del (i);
    }
}

static int 
Note_column_compare (Note_column *const&n1 , Note_column* const&n2)
{
  return Item::left_right_compare (n1, n2);
}

bool
Slur::broken_edge_b ( Direction dir) const
{
  return extrema ()[dir] != spanned_drul_[dir];
}

bool
Slur::normal_edge_b ( Direction dir) const
{
  Note_column *n = extrema ()[dir];
  return !broken_edge_b ( dir)
    && n->stem_l_
    && n->stem_l_->get_elt_property (transparent_scm_sym) == SCM_BOOL_F
    && n->head_l_arr_.size ();
}

Drul_array<Note_column*>
Slur::extrema ()const
{
  Drul_array<Note_column*> extrema;
  extrema[LEFT] = encompass_arr_[0];
  extrema[RIGHT] = encompass_arr_.top ();
  return extrema;
}

/*
  TODO.

  Unhair this.
 */
void
Slur::do_post_processing ()
{
  encompass_arr_.sort (Note_column_compare);
  if (!dir_)
    dir_ = get_default_dir ();

  Real interline_f = paper_l ()->get_realvar (interline_scm_sym);
  Real internote_f = interline_f / 2;
  // URG
  Real notewidth_f = paper_l ()->note_width () * 0.8;

  /* 
   [OSU]: slur and tie placement

   slurs:
   * x = center of head (upside-down: inner raakpunt stem) - d * gap

   * y = length < 5ss : horizontal raakpunt + d * 0.25 ss
     y = length >= 5ss : y next interline - d * 0.25 ss
     --> height <= 5 length ?? we use <= 3 length, now...
   */
  
  Real gap_f = paper_l ()->get_var ("slur_x_gap");


  Direction d=LEFT;
 
  do 
    {
      if (broken_edge_b (d))
	{
	  // ugh -- check if needed
	  dx_f_drul_[d] = -d 
	    *(spanned_drul_[d]->extent (X_AXIS).length () - 0.5 * notewidth_f);

	  // prebreak
	  if (d == RIGHT)
	    {
	      dx_f_drul_[LEFT] = spanned_drul_[LEFT]->extent (X_AXIS).length ();
	      
	      // urg -- check if needed
	      if (encompass_arr_.size () > 1)
		dx_f_drul_[RIGHT] += notewidth_f;
	    }
	}
      /*
        normal slur
       */
      else if (normal_edge_b (d))
        {
	  Real notewidth_f = extrema ()[d]->extent (X_AXIS).length ();
	  dy_f_drul_[d] = (int)rint (extrema ()[d]->stem_l_-> extent (Y_AXIS)[dir_]);
	  dx_f_drul_[d] += 0.5 * notewidth_f - d * gap_f;
	  if (dir_ == extrema ()[d]->stem_l_->dir_)
	    {
	      if (dir_ == d)
		dx_f_drul_[d] += 0.5 * dir_ * notewidth_f;
	      else
		dx_f_drul_[d] += 0.25 * dir_ * notewidth_f;
	    }
	}
	else 
	  {
	    Real notewidth_f = extrema ()[d]->extent (X_AXIS).length ();
	    dy_f_drul_[d] = (int)rint (extrema ()[d]->head_positions_interval ()
				       [dir_]) * internote_f;
	    dx_f_drul_[d] += 0.5 * notewidth_f - d * gap_f;
	}
	dy_f_drul_[d] += dir_ * interline_f;
	if (extrema ()[d]->stem_l_ && (dir_ == extrema ()[d]->stem_l_->dir_))
	  dy_f_drul_[d] -= dir_ * internote_f;
      }
  while (flip(&d) != LEFT);

  // now that both are set, do dependent
  do 
    {
      if (broken_edge_b (d))
        {
	  Direction u = d;
	  flip(&u);

	  // postbreak
	  if (d == LEFT)
	    dy_f_drul_[u] += dir_ * internote_f;

	  dy_f_drul_[d] = dy_f_drul_[u];
	}
     }
  while (flip(&d) != LEFT);

  /*
    Slur should follow line of music
   */
  if (normal_edge_b (LEFT)
      && normal_edge_b (RIGHT)
      && (extrema ()[LEFT]->stem_l_ != extrema ()[RIGHT]->stem_l_))
    {
      Real note_dy = extrema ()[RIGHT]->stem_l_->head_positions ()[dir_]
	- extrema ()[LEFT]->stem_l_->head_positions ()[dir_];
      Real dy = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];
      /*
	Should we always follow note-heads, (like a tie)?
	For now, only if the note_dy != slur_dy, we'll do
	slur_dy := note_dy * factor.
      */
      if (sign (dy) != sign (note_dy))
	{
	  Real damp_f = paper_l ()->get_var ("slur_slope_follow_music_factor");
	  Real realdy = note_dy * damp_f;
	  Direction adjust_dir = (Direction)(- dir_ * sign (realdy));
	  if (!adjust_dir)
	    adjust_dir = -dir_;
	  /*
	    adjust only if no beam gets in the way
	   */
	  if (!extrema ()[adjust_dir]->stem_l_->beam_l_
	      || (adjust_dir == extrema ()[adjust_dir]->stem_l_->dir_)
	      || (extrema ()[adjust_dir]->stem_l_->beams_i_drul_[-adjust_dir] < 1))
	    {
	      dy_f_drul_[adjust_dir] = dy_f_drul_[-adjust_dir]
		+ 2 * adjust_dir * realdy;
	      Real dx = notewidth_f / 2;
	      if (adjust_dir != extrema ()[adjust_dir]->stem_l_->dir_)
		dx /= 2;
	      dx_f_drul_[adjust_dir] -= adjust_dir * dx;
	    }
	}
    }

  /*
    Avoid too steep slurs.
   */
  Real damp_f = paper_l ()->get_var ("slur_slope_damping");
  Offset d_off = Offset (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT],
    dy_f_drul_[RIGHT] - dy_f_drul_[LEFT]);
  d_off[X_AXIS] += extent (X_AXIS).length ();

  Real ratio_f = abs (d_off[Y_AXIS] / d_off[X_AXIS]);
  if (ratio_f > damp_f)
    dy_f_drul_[(Direction)(- dir_ * sign (d_off[Y_AXIS]))] +=
      dir_ * (ratio_f - damp_f) * d_off[X_AXIS];
}

Array<Offset>
Slur::get_encompass_offset_arr () const
{
  Real notewidth = paper_l ()->note_width () * 0.8;
  Real gap = paper_l ()->get_var ("slur_x_gap");

  /*
  urg.  Calcs done wrt the leftmost note.  Fixme.

  Calcs ignore possibility of pre/postbreak.


  */

  Offset left = Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]);
  left[X_AXIS] += encompass_arr_[0]->stem_l_->hpos_f ();

  Real internote = encompass_arr_[0]->stem_l_->staff_line_leading_f ()/2.0;

  /*
    <URG>
    i don't understand these two, but *must* for symmetry 
    look at encompass array: 
       lilypond -D input/test/slur-symmetry*.ly
       lilypond -D input/test/sleur.ly

    do_post_processing should have calculated these into
    dx_f_drul_[], no??

   */

  if (dir_ != encompass_arr_[0]->stem_l_->dir_)
    left[X_AXIS] += - 0.5 * notewidth * encompass_arr_[0]->stem_l_->dir_
      + gap;
  else if (encompass_arr_[0]->stem_l_->dir_ == UP)
    left[X_AXIS] -= notewidth;

  if ((dir_ == encompass_arr_[0]->stem_l_->dir_) 
    && (encompass_arr_[0]->stem_l_->dir_ == DOWN))
    left[Y_AXIS] -= internote * encompass_arr_[0]->stem_l_->dir_;
  /* </URG> */

  Offset d = Offset (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT],
    dy_f_drul_[RIGHT] - dy_f_drul_[LEFT]);
  d[X_AXIS] += extent (X_AXIS).length ();

  int first = 1;
  int last = encompass_arr_.size () - 1;

  // prebreak
  if (broken_edge_b (RIGHT))
    last++;

  // postbreak
  if (broken_edge_b (LEFT))
    first--;

  Array<Offset> notes;
  notes.push (Offset (0,0));

  Real dy =0.0;
  for (int i = 0; i < last; i++)
    {
      Encompass_info info (encompass_arr_[i], dir_, this);
      if (i >= first)
	notes.push (info.o_ - left);
      else
	dy = info.interstaff_f_;
    }

  notes[0][Y_AXIS] += dy;
  notes.push (d);
  
  return notes;
}


Array<Rod>
Slur::get_rods () const
{
  Array<Rod> a;
  Rod r;
  r.item_l_drul_ = spanned_drul_;
  r.distance_f_ = paper_l ()->get_var ("slur_x_minimum");

  a.push (r);
  return a;
}

