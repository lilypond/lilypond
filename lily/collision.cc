/*
  collision.cc -- implement Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "collision.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "paper-def.hh"
#include "axis-group-interface.hh"


Collision::Collision()
{
  Axis_group_interface (this).set_interface ();
  Axis_group_interface (this).set_axes (X_AXIS, Y_AXIS);
}

void
Collision::add_column (Note_column* ncol_l)
{
  Axis_group_interface (this).add_element (ncol_l);
  add_dependency (ncol_l);
}

/*
  UGH.  junk Shift_tup .
 */

void
Collision::before_line_breaking ()
{
  Array<Shift_tup> autos (automatic_shift ());
  Array<Shift_tup> hand (forced_shift ());
  Link_array<Score_element> done;
  
  Real wid = paper_l ()->get_var ("collision_note_width");
  for (int i=0; i < hand.size (); i++)
    {
      hand[i].e1_->translate_axis (hand[i].e2_ *wid, X_AXIS);
      done.push (hand[i].e1_);
    }

  for (int i=0; i < autos.size (); i++)
    {
      if (!done.find_l (autos[i].e1_))
	autos[i].e1_->translate_axis (autos[i].e2_ * wid, X_AXIS);
    }
}

/** This complicated routine moves note columns around horizontally to
  ensure that notes don't clash.

  This should be done better, probably.

  */
Array< Shift_tup >
Collision::automatic_shift ()
{
  Drul_array<Link_array<Note_column> > clash_groups;
  Drul_array<Array<int> > shifts;
  Array<Shift_tup>  tups;


  SCM s = get_elt_property ("elements");
  for (; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM car = gh_car (s);

      Score_element * se = unsmob_element (car);
      if (Note_column * col = dynamic_cast<Note_column*> (se))
	clash_groups[col->dir ()].push (col);
    }

  
  Direction d = UP;
  do
    {
      Array<int> & shift (shifts[d]);
      Link_array<Note_column> & clashes (clash_groups[d]);

      clashes.sort (Note_column::shift_compare);

      for (int i=0; i < clashes.size (); i++)
	{
	  SCM sh
	    = clashes[i]->remove_elt_property ("horizontal-shift");

	  if (gh_number_p (sh))
	    shift.push (gh_scm2int (sh));
	  else
	    shift.push (0);
	}
      
      for (int i=1; i < shift.size (); i++)
	{
	  if (shift[i-1] == shift[i])
	    {
	      warning (_ ("Too many clashing notecolumns.  Ignoring them."));
	      return tups;
	    }
	}
    }
  while ((flip (&d))!= UP);

  Drul_array< Array < Slice > > extents;
  Drul_array< Array < Real > > offsets;
  d = UP;
  do
    {
      for (int i=0; i < clash_groups[d].size (); i++)
	{
	  Slice s(clash_groups[d][i]->head_positions_interval ());
	  s[LEFT] --;
	  s[RIGHT]++;
	  extents[d].push (s);
	  offsets[d].push (d * 0.5 * i);
	}
    }
  while ((flip (&d))!= UP);
  
  do
    {
      for (int i=1; i < clash_groups[d].size (); i++)
	{
	  Slice prev =extents[d][i-1];
	  prev.intersect (extents[d][i]);
	  if (prev.length ()> 0 ||
	      (extents[-d].size () && d * (extents[d][i][-d] - extents[-d][0][d]) < 0))
	    for (int j = i; j <  clash_groups[d].size (); j++)
	      offsets[d][j] += d * 0.5;
	}
    }	
  while ((flip (&d))!= UP);

  /*
    if the up and down version are close, and can not be merged, move
    all of them again. */
  if (extents[UP].size () && extents[DOWN].size ())
    {
      Note_column *cu_l =clash_groups[UP][0];
      Note_column *cd_l =clash_groups[DOWN][0];


      /*
	TODO.
       */
      Note_head * nu_l= cu_l->first_head();
      Note_head * nd_l = cd_l->first_head();
      
      int downpos = 	cd_l->head_positions_interval ()[BIGGER];
      int uppos = 	cu_l->head_positions_interval ()[SMALLER];      
      
      bool merge  =
	downpos == uppos
	&& nu_l->balltype_i () == nd_l->balltype_i ();


      if (!to_boolean (get_elt_property ("merge-differently-dotted")))
	merge = merge && nu_l->dot_count () == nd_l->dot_count ();

      /*
	notes are close, but can not be merged.  Shift
       */
      if (abs(uppos - downpos) < 2 && !merge)
	  do
	  {
	    for (int i=0; i < clash_groups[d].size (); i++)
	      {
		offsets[d][i] -= d * 0.5;
	      }
	  }
	  while ((flip (&d))!= UP);
    }

  do
    {
      for (int i=0; i < clash_groups[d].size (); i++)
	tups.push (Shift_tup (clash_groups[d][i], offsets[d][i]));
    }
  while (flip (&d) != UP);
  return tups;
}


Array <Shift_tup>
Collision::forced_shift ()
{
  Array<Shift_tup> tups;
  
  SCM s = get_elt_property ("elements");
  for (; gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * se = unsmob_element ( gh_car (s));

      SCM force =  se->remove_elt_property ("force-hshift");
      if (gh_number_p (force))
	{
	  tups. push (Shift_tup (se, gh_scm2double (force)));
	}
    }
  return tups;
}



