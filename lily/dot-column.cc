/*
  dot-column.cc -- implement Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dots.hh"
#include "dot-column.hh"
#include "rhythmic-head.hh"

void
Dot_column::add_dots (Dots *d)
{
  dot_l_arr_.push (d);
  add_dependency (d);
  add_element (d);
}

void
Dot_column::add_head (Rhythmic_head *r)
{
  if (!r->dots_l_)
    return ;

  add_support (r);
  add_dots (r->dots_l_);
}

void
Dot_column::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  Note_head_side::do_substitute_element_pointer (o,n);
  if (Dots * d = dynamic_cast<Dots*> (o))
    dot_l_arr_.substitute (d, dynamic_cast<Dots*> (n));
}

int
Dot_column::compare (Dots * const &d1, Dots * const &d2)
{
  return int (d1->position_f () - d2->position_f ());
}

void
Dot_column::do_pre_processing ()
{
  dot_l_arr_.sort (Dot_column::compare);
  Note_head_side::do_pre_processing ();
}

Dot_column::Dot_column ()
{
  set_direction (RIGHT);
  set_axes(X_AXIS,X_AXIS);
}

/*
  Will fuck up in this case.

  X.  .
  X.X .
   |X .
   |
   |
   |X .
   |
   |


   Should be smarter.
 */
void
Dot_column::do_post_processing ()
{
  if (dot_l_arr_.size () < 2)
    return;
  Slice s;
  s.set_empty ();

  Array<int> taken_posns;
  int conflicts = 0;
  for (int i=0; i < dot_l_arr_.size (); i++)
    {
      for (int j=0; j < taken_posns.size (); j++)
	if (taken_posns[j] == (int) dot_l_arr_[i]->position_f ())
	  conflicts++;
      taken_posns.push ((int)dot_l_arr_[i]->position_f ());
      s.unite (Slice ((int)dot_l_arr_[i]->position_f (),
		      (int)dot_l_arr_[i]->position_f ()));      
    }

  if (!conflicts)
    return;
  
  int  middle = s.center ();
  /*
    +1 -> off by one 
   */
  int pos = middle - dot_l_arr_.size () + 1;
  if (!(pos % 2))
    pos ++;			// center () rounds down.

  for (int i=0; i  <dot_l_arr_.size (); pos += 2, i++)
    {
      dot_l_arr_[i]->set_position(pos);
    }
}
