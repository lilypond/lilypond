/*
  collision.cc -- implement Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "collision.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "paper-def.hh"

Collision::Collision()
{
  set_axes (X_AXIS, Y_AXIS);
}

void
Collision::add_column (Note_column* ncol_l)
{
  clash_l_arr_.push (ncol_l);
  add_element (ncol_l);
  add_dependency (ncol_l);
}
/**
  should derive of Array.
 */
static
int idx (int dir, bool h_shift_b)
{
  assert (abs (dir) == 1);
  int j = dir > 0 ? 0 : 3;
  if (h_shift_b)
    j += dir;
  return j;
}

/** This complicated routine moves note columns around horizontally
  (and rests vertically) to ensure that notes don't clash.

  This should be done better, probably.

  This routine is dedicated to Stine Randmael :-)

  */
void
Collision::do_pre_processing()
{
  if (clash_l_arr_.size() <= 1)
    return;

  /*
    [stem up, stem up shifted, stem down shifted, stem down]
  */
  Array<Note_column*> clash_group_arr_a[4];

  for (int i=0; i < clash_l_arr_.size(); i++)
    {
      Note_column* c_l = clash_l_arr_[i];
      Direction d = c_l->dir ();
      if (!d)
	{
	  warning (_ ("No stem direction set. Ignoring column in clash."));
	  continue;
	}

      SCM shift = c_l->remove_elt_property (horizontal_shift_scm_sym);
      bool shift_b  = (shift != SCM_BOOL_F);
      clash_group_arr_a[idx (d, shift_b)].push (c_l);
    }


  for (int j=0; j < 4; j++)
    {
      if (clash_group_arr_a[j].size() > 1)
	{
	  warning (_ ("Too many clashing notecolumns. Ignoring them."));
	  return;
	}
    }
  Direction d = UP;
  do
    {
      if (!clash_group_arr_a[idx (d, false)].size())
	{
	  clash_group_arr_a[idx (d,  false)] = clash_group_arr_a[idx (d, true)];
	  clash_group_arr_a[idx (d, true)].clear();
	}
    }
  while (flip (&d) != UP);


  Interval_t<int> y_extent[4];
  Note_column * col_l_a[4];
  Real x_off [4];

  for (int j =0 ; j < 4; j++)
    {
      if (clash_group_arr_a[j].size())
	col_l_a[j] = clash_group_arr_a[j][0];
      else
	col_l_a[j] = 0;

      if (col_l_a[j])
	{
	  y_extent[j] = col_l_a[j]->head_positions_interval();
	}


      x_off [j] = 0.0;
    }

  do
    {
      x_off[idx (d, true)] = d*0.5;
    }
  while (flip (&d) != UP);


  // y_extent: smallest y-pos noteball interval containing all balls
  // 4 (0..3) groups: stem up/down; shift on/off;
  Interval_t<int> middle (y_extent[idx (-1,0)][BIGGER],
			  y_extent[idx (1,0)][SMALLER]);
  Interval_t<int> open_middle (y_extent[idx (-1,0)][BIGGER]+1, y_extent[idx (1,0)][SMALLER]-1);
  do
    {
      if (!open_middle.contains_b (y_extent[idx (d,true)]))
	x_off[idx (d, true)] = d *1.0 ;
    } while ((d *= -1) != 1);


  if (!middle.empty_b()
      && middle.length() < 2 && col_l_a[idx (1,0)] && col_l_a[idx (-1,0)])
    {
      // reproduction of bugfix at 3am ?
      Note_head * nu_l= col_l_a[idx (1,0)]->head_l_arr_[0];
      Note_head * nd_l = col_l_a[idx (-1,0)]->head_l_arr_.top();
      if (! (nu_l->balltype_i_ == nd_l->balltype_i_
	     && nu_l->dots_i_ == nd_l->dots_i_  && middle.length() == 0))
	{
	  do
	    {
	      x_off[idx (d, false)] -= d*0.5;
	      x_off[idx (d, true)] -= d*0.5;
	    }
	  while (flip (&d) != UP);
	}
    }

  Real wid_f = paper_l ()->note_width ();
  for (int j=0; j < 4; j++)
    {
      if (col_l_a[j])
	{
	  Offset o (x_off[j] * wid_f, 0);
	  col_l_a[j]->translate (o);
	}
    }
}




void
Collision::do_substitute_element_pointer (Score_element*o_l,Score_element*n_l)
{
  if (o_l)
    {
      clash_l_arr_.substitute (dynamic_cast<Note_column *> (o_l),
			       dynamic_cast <Note_column *> (n_l));

    }
}
