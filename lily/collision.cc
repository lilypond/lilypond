/*
  collision.cc -- implement Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "debug.hh"
#include "collision.hh"
#include "note-column.hh"
#include "rhythmic-head.hh"
#include "paper-def.hh"
#include "axis-group-interface.hh"
#include "item.hh"

void
Collision::add_column (Score_element*me,Note_column* ncol_l)
{
  ncol_l->add_offset_callback (force_shift_callback, X_AXIS);
  Axis_group_interface::add_element (me, ncol_l);
  me->add_dependency (ncol_l);
}

Real
Collision::force_shift_callback (Score_element * c, Axis a)
{
  assert (a == X_AXIS);
  
  Score_element * me = c->parent_l (a);
  /*
    ugh. the way DONE is done is not clean
   */
  if (!unsmob_element (me->get_elt_property ("done")))
    {
      me->set_elt_property ("done", me->self_scm_);
      do_shifts (me);
    }
  
  return 0.0;
}

/*
  TODO: make callback of this.
 */
void
Collision::do_shifts(Score_element* me)
{
  SCM autos (automatic_shift (me));
  SCM hand (forced_shift (me));
  
  Link_array<Score_element> done;
  
  Real wid = me->paper_l ()->get_var ("collision_note_width"); // elt prop
  for (; gh_pair_p (hand); hand =gh_cdr (hand))
    {
      Score_element * s = unsmob_element (gh_caar (hand));
      Real amount = gh_scm2double (gh_cdar (hand));
      
      s->translate_axis (amount *wid, X_AXIS);
      done.push (s);
    }
  for (; gh_pair_p (autos); autos =gh_cdr (autos))
    {
      Score_element * s = unsmob_element (gh_caar (autos));
      Real amount = gh_scm2double (gh_cdar (autos));
      
      if (!done.find_l (s))
	s->translate_axis (amount * wid, X_AXIS);
    }
}

/** This complicated routine moves note columns around horizontally to
  ensure that notes don't clash.

  This should be put into Scheme.  
  */
SCM
Collision::automatic_shift (Score_element *me)
{
  Drul_array<Link_array<Note_column> > clash_groups;
  Drul_array<Array<int> > shifts;
  SCM  tups = SCM_EOL;

  SCM s = me->get_elt_property ("elements");
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
	    = clashes[i]->get_elt_property ("horizontal-shift");

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
	  Slice s(Note_column::head_positions_interval (clash_groups[d][i]));
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
      Score_element * nu_l= cu_l->first_head();
      Score_element * nd_l = cd_l->first_head();
      
      int downpos = Note_column::head_positions_interval (cd_l)[BIGGER];
      int uppos = Note_column::head_positions_interval (cu_l)[SMALLER];      
      
      bool merge  =
	downpos == uppos
	&& Rhythmic_head::balltype_i (nu_l) == Rhythmic_head::balltype_i (nd_l);


      if (!to_boolean (me->get_elt_property ("merge-differently-dotted")))
	merge = merge && Rhythmic_head::dot_count (nu_l) == Rhythmic_head::dot_count (nd_l);

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
	tups = gh_cons (gh_cons (clash_groups[d][i]->self_scm_, gh_double2scm (offsets[d][i])),
				 tups);
    }
  while (flip (&d) != UP);
  return tups;
}


SCM
Collision::forced_shift (Score_element *me)
{
  SCM tups = SCM_EOL;
  
  SCM s = me->get_elt_property ("elements");
  for (; gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * se = unsmob_element (gh_car (s));

      SCM force =  se->remove_elt_property ("force-hshift");
      if (gh_number_p (force))
	{
	  tups = gh_cons (gh_cons (se->self_scm_, force),
			  tups);
	}
    }
  return tups;
}



