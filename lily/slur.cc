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



void
Slur::add (Note_column*n)
{
  encompass_arr_.push (n);
  add_dependency (n);
}

void
Slur::set_default_dir()
{
  dir_ = DOWN;
  for (int i=0; i < encompass_arr_.size(); i ++) 
    {
      if (encompass_arr_[i]->dir_ < 0) 
	{
	  dir_ =UP;
	  break;
	}
    }
}

void
Slur::do_add_processing()
{
  set_bounds(LEFT, encompass_arr_[0]);    
  if (encompass_arr_.size () > 1)
    set_bounds(RIGHT, encompass_arr_.top());
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
  while ((i = encompass_arr_.find_i ((Note_column*)o->item())) >=0) 
    {
      if (n)
	encompass_arr_[i] = (Note_column*)n->item();
      else
	encompass_arr_.del (i);
    }
}


static int 
Note_column_compare (Note_column *const&n1 , Note_column* const&n2)
{
  return Item::left_right_compare(n1, n2);
}

void
Slur::do_post_processing()
{
  encompass_arr_.sort (Note_column_compare);
  if (!dir_)
    set_default_dir();
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
  extrema[RIGHT] = encompass_arr_.top();

  Direction d=LEFT;
  Real nw_f = paper()->note_width ();
 
  do 
    {
      if  (extrema[d] != spanned_drul_[d]) 
	{
	  dx_f_drul_[d] = -d 
	    *(spanned_drul_[d]->width ().length () -0.5*nw_f);
	}
      else if (extrema[d]->stem_l_ && !extrema[d]->stem_l_->transparent_b_) 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->stem_l_->height()[dir_]);
	  /* normal slur from notehead centre to notehead centre, minus gap */
	  dx_f_drul_[d] += -d * gap_f;
	}
      else 
        {
	  dy_f_drul_[d] = (int)rint (extrema[d]->head_positions_interval()[dir_])* inter_f;
	}
      dy_f_drul_[d] += dir_ * interline_f;
    }
  while ((d *= -1) != LEFT);
}

IMPLEMENT_IS_TYPE_B1(Slur,Spanner);
