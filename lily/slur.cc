/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
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

//IMPLEMENT_IS_TYPE_B1(Slur,Spanner);
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
  Real interline_f = paper ()->interline_f ();
  Real inter_f = interline_f / 2;

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
  Real gap_f = interline_f / 2; // 5;
  
  Drul_array<Note_column*> extrema;
  extrema[LEFT] = encompass_arr_[0];
  extrema[RIGHT] = encompass_arr_.top ();

  Direction d=LEFT;
  Real nw_f = paper ()->note_width ();
 
  do 
    {
      if (extrema[d] != spanned_drul_[d]) 
	{
	  dx_f_drul_[d] = -d 
	    *(spanned_drul_[d]->width ().length () -0.5*nw_f);
	  Direction u = d;
	  flip(&u);
	  if ((extrema[u] == spanned_drul_[u]) && extrema[u]->stem_l_)
	    {
	      dy_f_drul_[d] = extrema[u]->stem_l_->height ()[dir_];
	      dy_f_drul_[u] = extrema[u]->stem_l_->height ()[dir_];
	    }
	}
      else if (extrema[d]->stem_l_ && !extrema[d]->stem_l_->transparent_b_) 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->stem_l_->height ()[dir_]);
	  dx_f_drul_[d] += 0.5 * nw_f - d * gap_f;
	  if (dir_ == extrema[d]->stem_l_->dir_)
	    {
	      if (dir_ == d)
		dx_f_drul_[d] += 0.5 * (dir_ * d) * d * nw_f;
	      else
		dx_f_drul_[d] += 0.25 * (dir_ * d) * d * nw_f;
	    }
	}
      else 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->head_positions_interval ()
	    [dir_])* inter_f;
	}
      dy_f_drul_[d] += dir_ * interline_f;
    }
  while (flip(&d) != LEFT);
}

Array<Offset>
Slur::get_encompass_offset_arr () const
{
  Real interline = paper ()->interline_f ();
  Real notewidth = paper ()->note_width ();
  Real internote = interline / 2;

  Stem* left_stem = encompass_arr_[0]->stem_l_;
  Real left_x = left_stem->hpos_f ();
  left_x += dx_f_drul_[LEFT];

  Real left_y = dy_f_drul_[LEFT];

  Real dx = width ().length ();
  dx += (dx_f_drul_[RIGHT] - dx_f_drul_[LEFT]);
  dx = dx <? 1000;
  dx = dx >? 2 * interline;

  Real dy = (dy_f_drul_[RIGHT] - dy_f_drul_[LEFT]);
  if (abs (dy) > 1000)
    dy = sign (dy) * 1000;

  int first = 1;
  int last = encompass_arr_.size () - 1;
  if (encompass_arr_[0] != spanned_drul_[LEFT])
    {
      first = 0;
      left_x = spanned_drul_[LEFT]->width ().length ();
      left_x -= 2 * notewidth;
      left_y = encompass_arr_[last]->stem_l_->height ()[dir_];
      dy = 0;
    }
  if (encompass_arr_.top () != spanned_drul_[RIGHT])
    {
      last += 1;
      dy = 0;
    }

#define RESIZE_ICE
#ifndef RESIZE_ICE
  Array<Offset> notes;
  notes.push (Offset (0,0));
#else
  int n = last - first + 2;
  Array<Offset> notes (n);
  notes[0] = Offset (0,0);
#endif
  for (int i = first; i < last; i++)
    {
      Stem* stem = encompass_arr_[i]->stem_l_;
      /* 
	set x to middle of notehead or on exact x position of stem,
	according to slur direction
	   */
      Real x = stem->hpos_f ();

      if (stem->dir_ != dir_)
	x += 0.5 * notewidth;
      else if (stem->dir_ == UP)
	x += 1.0 * notewidth;

      x -= left_x;

      Real y = stem->height ()[dir_];

      /*
	leave a gap: slur mustn't touch head/stem
       */
      y += 2.5 * internote * dir_;

      if (dir_ == DOWN)
	y += 1.5 * internote * dir_;

      y -= left_y;

#ifndef RESIZE_ICE
      notes.push (Offset (x, y));
    }
  notes.push (Offset (dx, dy));
#else
      notes[i - first + 1] = Offset (x, y);
    }
  notes[n - 1] = Offset (dx, dy);
#endif

  return notes;
}

