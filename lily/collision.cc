/*
  collision.cc -- implement Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "collision.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "paper-def.hh"

Collision::Collision()
{
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
      if (! c_l->dir_)
	{
	  warning (_ ("No stem direction set. Ignoring column in clash."));
	  continue;
	}
      int d = (c_l->dir_);

      clash_group_arr_a[idx (d, c_l->h_shift_b_)].push (c_l);
    }


  for (int j=0; j < 4; j++)
    {
      if (clash_group_arr_a[j].size() > 1)
	{
	  warning (_ ("Too many clashing notecolumns. Ignoring them."));
	  return;
	}
    }
  int d = 1;
  do
    {
      if (!clash_group_arr_a[idx (d, false)].size())
	{
	  clash_group_arr_a[idx (d,  false)] = clash_group_arr_a[idx (d, true)];
	  clash_group_arr_a[idx (d, true)].clear();
	}
    }
  while ((d *= -1) != 1);


  Interval_t<int> y_extent[4];
  Note_column * col_l_a[4];
  Real x_off [4];
  int y_off[4];

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
      y_off[j] = 0;
    }

  do
    {
      x_off[idx (d, true)] = d*0.5;
    }
  while ((d *= -1) != 1);


  // y_extent: smallest y-pos noteball interval containing all balls
  // 4 (0..3) groups: stem up/down; shift on/off;
  Interval_t<int> middle (y_extent[idx (-1,0)].max(),
			  y_extent[idx (1,0)].min());
  Interval_t<int> open_middle (y_extent[idx (-1,0)].max()+1, y_extent[idx (1,0)].min ()-1);
  do
    {
      if (!open_middle.contains_b (y_extent[idx (d,true)]))
	x_off[idx (d, true)] = d *1.0 ;
    } while ((d *= -1) != 1);

  if (!middle.empty_b()
      && middle.length() < 2 && col_l_a[idx (1,0)] && col_l_a[idx (-1,0)]) {
    // reproduction of bugfix at 3am ?
    Note_head * nu_l= col_l_a[idx (1,0)]->head_l_arr_[0];
    Note_head * nd_l = col_l_a[idx (-1,0)]->head_l_arr_.top();
    if (! (nu_l->balltype_i_ == nd_l->balltype_i_
	   && nu_l->dots_i_ == nd_l->dots_i_  && middle.length() == 0))
      {
	x_off[idx (1,0)] -= 0.5;
	x_off[idx (1,1)] -= 0.5;
	x_off[idx (-1,1)] += 0.5;
	x_off[idx (-1,0)] += 0.5;
      }

  }
  Real inter_f = paper()->internote_f ();
  Real wid_f = paper()->note_width ();
  for (int j=0; j < 4; j++)
    {
      if (col_l_a[j])
	{
	  /* collision.cc:138: request for method `translate' is ambiguous

	     (shaddup)
	     */
	  Offset o (x_off[j] * wid_f, y_off[j] * inter_f);
	  ((Score_element*)col_l_a[j])->translate (o);
	}
    }
}


IMPLEMENT_IS_TYPE_B1(Collision, Item);

void
Collision::do_substitute_dependency (Score_element*o_l,Score_element*n_l)
{
  clash_l_arr_.substitute ((Note_column*)dynamic_cast <Item *> (o_l),
			   (Note_column*)(n_l?dynamic_cast <Item *> (n_l):0));
}
