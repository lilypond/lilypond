/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

/*
  TODO:
  
  think about crossing stems.
  Begin and end should be treated as a Script.
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
      if  (extrema[d] != spanned_drul_[d]) 
	{
	  dx_f_drul_[d] = -d 
	    *(spanned_drul_[d]->width ().length () -0.5*nw_f);
	}
      else if (extrema[d]->stem_l_ && !extrema[d]->stem_l_->transparent_b_) 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->stem_l_->height ()[dir_]);
	  /* normal slur from notehead centre to notehead centre, minus gap */
	  dx_f_drul_[d] += -d * gap_f;
	}
      else 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->head_positions_interval ()[dir_])* inter_f;
	}
      dy_f_drul_[d] += dir_ * interline_f;
    }
  while (flip(&d) != LEFT);
}

static Real
pos_correct (Real x, Real dx, Real dy)
{
  /*
    guess how much we can safely increase 'h'
    parameter of bow without taking too large
    or too many steps.
    empiric computer science
   */
//  return (1.0 + 2.0 * x / dx) / 4.0;
  return 1.0;
}

Real
Slur::height_f () const
{
  Real interline = paper ()->interline_f ();
  Real notewidth = paper ()->note_width ();
  Real internote = interline / 2;

  /*
   having the correct (i.e. exactly the same as the ps stuff)
   x,y coordinates is essential.
   getting them is quite a mess, currently.
   */

  Stem* left_stem =encompass_arr_[0]->stem_l_; 
  Real left_x = left_stem->hpos_f ();
  // ugh, do bow corrections (see brew_mol)
  left_x += dx_f_drul_[LEFT] + 0.5 * notewidth;

//  Real left_y = left_stem->dir_ == dir_ ? left_stem->stem_end_f () + dir_
//    : left_stem->stem_begin_f () + 0.5 * dir_;
//  left_y *= internote;
  // ugh, do bow corrections (see brew_mol)
//  left_y = dy_f_drul_[LEFT];

  // ugh, do bow corrections (see brew_mol)
  Real left_y = dy_f_drul_[LEFT];

  // ugh, where does this asymmetry come from?
  if (dir_ == DOWN)
    left_y -= dir_ * internote;

  Real dx = width ().length ();
  Real dy = (dy_f_drul_[RIGHT] - dy_f_drul_[LEFT]);
  Real centre_x = dx / 2;


  // ugh, need staffheight for bow damping
  Bezier_bow b (paper ());

  if (check_debug && !monitor->silent_b ("Slur"))
    {
      cout << "*****************" << endl;
      cout << "dir: " << (int)dir_ << endl;
      cout << "dx: " << dx << endl;
      cout << "dy: " << dy << endl;
      cout << "centre: " << centre_x << endl;
      for (int i = 0; i < encompass_arr_.size (); i++)
	cout << "i: " << i << " x: " 
	    << encompass_arr_[i]->stem_l_->hpos_f () - left_x << endl;
    }
  Real height = 0;
  Real dh = 0;
  do
    {
      height += dh;
      dh = 0;
      b.calc (dx, dy, height, dir_);
      
      if (check_debug && !monitor->silent_b ("Slur"))
        cout << "----" << endl;
      for (int i = 1; i < encompass_arr_.size () - 1; i++) 
	{
	  Stem* stem = encompass_arr_[i]->stem_l_;
	  /* 
	    set x to middle of notehead or on exact x position of stem,
	    according to slur direction
	   */
	  Real x = stem->hpos_f () - left_x + notewidth / 2;
	  if (stem->dir_ != dir_)
	    x += notewidth / 2;
	  else if (stem->dir_ == UP)
	    x += notewidth;
	  Real y = stem->dir_ == dir_ ? stem->stem_end_f ()
	    : stem->stem_begin_f () + 1.5 * dir_;

	  /*
	    leave a gap: slur mustn't touch head/stem
	   */
	  y += 2.5 * dir_;
	  y *= internote;
	  y -= left_y;

	  Real shift = dir_ * (y - b.y (x));

	  if (check_debug && !monitor->silent_b ("Slur"))
	    {
	      cout << "x: " << x << " (" << abs (centre_x - x) << ")" << endl;
	      cout << "y: " << y << ", curve: " << b.y (x) << endl;
	      cout << "shift: " << shift << endl;
	    }

	  if (shift > 0.1 * internote)
	    dh = dh >? shift * pos_correct (abs (centre_x - x), dx, dy);
	}
    } while (dh);

  return height;
}

IMPLEMENT_IS_TYPE_B1(Slur,Spanner);
