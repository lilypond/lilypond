/*
  collision.cc -- implement Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "collision.hh"
#include "note-column.hh"
#include "rhythmic-head.hh"
#include "paper-def.hh"
#include "axis-group-interface.hh"
#include "item.hh"
#include "stem.hh"

MAKE_SCHEME_CALLBACK (Collision,force_shift_callback,2);

SCM
Collision::force_shift_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == X_AXIS);
  
   me = me->get_parent (a);

   if (! to_boolean (me->get_grob_property ("collision-done")))
    {
      me->set_grob_property ("collision-done", SCM_BOOL_T);
      do_shifts (me);
    }
  
  return gh_double2scm (0.0);
}


void
check_meshing_chords (Grob*me,
		      Drul_array< Array < Real > > *offsets,
		      Drul_array< Array < Slice > > const &extents,
		      Drul_array<Link_array<Grob> > const &clash_groups)
	
{
  if (!extents[UP].size () || ! extents[DOWN].size ())
    return ;
  
  
  Grob *cu =clash_groups[UP][0];
  Grob *cd =clash_groups[DOWN][0];

  Grob * nu_l= Note_column::first_head (cu);
  Grob * nd_l = Note_column::first_head (cd);
      
     

  /*
    this case (distant half collide), 
    
        |
      x |
     | x
     |

   the noteheads may be closer than this case (close half collide)

       |
       |
      x 
     x
    |
    |
    
   */
  
  bool close_half_collide = false;
  bool distant_half_collide = false;  
  bool full_collide = false;  

  /*
    TODO:

    filter out the 'o's in this configuration, since they're no part
    in the collision.

     |
    x|o
    x|o
    x

    
   */
  Array<int> ups = Stem::note_head_positions (Note_column::stem_l (cu));
  Array<int> dps = Stem::note_head_positions (Note_column::stem_l (cd));

  /*
    they're too far apart to collide. 
    
   */

  if (ups[0] > dps.top () + 1)
    return ; 

  bool touch = (ups[0] - dps.top () >= 0);
  
  bool merge_possible = (ups[0] >= dps[0]) && (ups.top () >= dps.top ());

  merge_possible = merge_possible &&
    Rhythmic_head::balltype_i (nu_l) == Rhythmic_head::balltype_i (nd_l);


  /*
    don't merge whole notes (or longer, like breve, longa, maxima) 
   */
  merge_possible = merge_possible && (Rhythmic_head::balltype_i (nu_l) > 0);

  if (!to_boolean (me->get_grob_property ("merge-differently-dotted")))
    merge_possible = merge_possible && Rhythmic_head::dot_count (nu_l) == Rhythmic_head::dot_count (nd_l);
  
  int i = 0, j=0;
  while (i < ups.size () && j < dps.size ())
  {
    if (abs (ups[i] - dps[j]) == 1)
      {
	merge_possible = false;
	if (ups[i] > dps[j])
	  close_half_collide = true;
	else
	  distant_half_collide = true;
      }
    else if (ups[i]==dps[j])
      full_collide = true;
    else if (ups[i] >dps[0] && ups[i] < dps.top ())
      merge_possible = false;
    else if (dps[j] >ups[0] && dps[j] < ups.top ())
      merge_possible = false;
    
    if (ups[i] < dps[j])
      i++;
    else if (ups[i] > dps[j])
      j++;
    else
      {
	i++;
	j++;
      }
  }

  Drul_array<Real> center_note_shifts;
  center_note_shifts[LEFT] = 0.0;
  center_note_shifts[RIGHT] = 0.0;

  
  Real shift_amount = 1;

  if (touch)
    shift_amount *= -1;

  /*
    for full collisions, the right hand head may obscure dots, so
    make sure the dotted heads go to the right.
   */
  if ((Rhythmic_head::dot_count (nu_l) > Rhythmic_head::dot_count (nd_l)
       && full_collide))
    shift_amount = 1;

  /*
    TODO: these numbers are magic; should devise a set of grob props
    to tune this behavior.  */
  
  if (merge_possible)
    shift_amount *= 0.0;
  else if (close_half_collide && !touch)
    shift_amount *= 0.52;
  else if (distant_half_collide && !touch)
    shift_amount *= 0.4;
  else if (distant_half_collide || close_half_collide || full_collide)
    shift_amount *= 0.5;
  
  /*
    we're meshing.
  */
  else if (Rhythmic_head::dot_count (nu_l) || Rhythmic_head::dot_count (nd_l))
    shift_amount *= 0.1;
  else
    shift_amount *= 0.25;

  Direction d = UP;
  do
    {
      for (int i=0; i < clash_groups[d].size (); i++)
	(*offsets)[d][i] += d * shift_amount;
    }
  while ((flip (&d))!= UP);
}


/*
  TODO: make callback of this.

  TODO:

  note-width is hardcoded, making it difficult to handle all note
  heads sanely. We should really look at the widths of the colliding
  columns, and have a separate setting for "align stems".

  
 */
void
Collision::do_shifts (Grob* me)
{
  SCM autos (automatic_shift (me));
  SCM hand (forced_shift (me));
  
  Link_array<Grob> done;


  Real wid
    = gh_scm2double (me->get_grob_property ("note-width"));
  
  for (; gh_pair_p (hand); hand =ly_cdr (hand))
    {
      Grob * s = unsmob_grob (ly_caar (hand));
      Real amount = gh_scm2double (ly_cdar (hand));
      
      s->translate_axis (amount *wid, X_AXIS);
      done.push (s);
    }
  for (; gh_pair_p (autos); autos =ly_cdr (autos))
    {
      Grob * s = unsmob_grob (ly_caar (autos));
      Real amount = gh_scm2double (ly_cdar (autos));
      
      if (!done.find_l (s))
	s->translate_axis (amount * wid, X_AXIS);
    }
}

/** This complicated routine moves note columns around horizontally to
  ensure that notes don't clash.

  This should be put into Scheme.  
  */
SCM
Collision::automatic_shift (Grob *me)
{
  Drul_array<Link_array<Grob> > clash_groups;
  Drul_array<Array<int> > shifts;
  SCM  tups = SCM_EOL;

  SCM s = me->get_grob_property ("elements");
  for (; gh_pair_p (s); s = ly_cdr (s))
    {
      SCM car = ly_car (s);

      Grob * se = unsmob_grob (car);
      if (Note_column::has_interface (se))
	clash_groups[Note_column::dir (se)].push (se);
    }

  
  Direction d = UP;
  do
    {
      Array<int> & shift (shifts[d]);
      Link_array<Grob> & clashes (clash_groups[d]);

      clashes.sort (Note_column::shift_compare);

      for (int i=0; i < clashes.size (); i++)
	{
	  SCM sh
	    = clashes[i]->get_grob_property ("horizontal-shift");

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
	  Slice s (Note_column::head_positions_interval (clash_groups[d][i]));
	  s[LEFT] --;
	  s[RIGHT]++;
	  extents[d].push (s);
	  offsets[d].push (d * 0.5 * i);
	}
    }
  while ((flip (&d))!= UP);

  /*
    do horizontal shifts of each direction 

       | 
      x||
       x||
        x|
   */
  
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
    Check if chords are meshing
   */

  check_meshing_chords (me, &offsets, extents, clash_groups);
  
  do
    {
      for (int i=0; i < clash_groups[d].size (); i++)
	tups = gh_cons (gh_cons (clash_groups[d][i]->self_scm (), gh_double2scm (offsets[d][i])),
				 tups);
    }
  while (flip (&d) != UP);
  return tups;
}


SCM
Collision::forced_shift (Grob *me)
{
  SCM tups = SCM_EOL;
  
  SCM s = me->get_grob_property ("elements");
  for (; gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * se = unsmob_grob (ly_car (s));

      SCM force =  se->remove_grob_property ("force-hshift");
      if (gh_number_p (force))
	{
	  tups = gh_cons (gh_cons (se->self_scm (), force),
			  tups);
	}
    }
  return tups;
}

void
Collision::add_column (Grob*me,Grob* ncol_l)
{
  ncol_l->add_offset_callback (Collision::force_shift_callback_proc, X_AXIS);
  Axis_group_interface::add_element (me, ncol_l);
  me->add_dependency (ncol_l);
}
