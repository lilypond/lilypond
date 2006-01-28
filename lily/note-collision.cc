/*
  collision.cc -- implement Collision

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "note-collision.hh"

#include "axis-group-interface.hh"
#include "dot-column.hh"
#include "international.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "warn.hh"


void
check_meshing_chords (Grob *me,
		      Drul_array<Array<Real> > *offsets,
		      Drul_array<Array<Slice> > const &extents,
		      Drul_array<Link_array<Grob> > const &clash_groups)

{
  if (!extents[UP].size () || ! extents[DOWN].size ())
    return;

  Grob *cu = clash_groups[UP][0];
  Grob *cd = clash_groups[DOWN][0];

  /* Every note column should have a stem, but avoid a crash. */
  if (!Note_column::get_stem (cu) || !Note_column::get_stem (cd))
    return;

  Grob *nu = Note_column::first_head (cu);
  Grob *nd = Note_column::first_head (cd);

  Array<int> ups = Stem::note_head_positions (Note_column::get_stem (cu));
  Array<int> dps = Stem::note_head_positions (Note_column::get_stem (cd));

  /* Too far apart to collide.  */
  if (ups[0] > dps.top () + 1)
    return;

  // FIXME: what's this?
  bool merge_possible = (ups[0] >= dps[0]) && (ups.top () >= dps.top ());

  /* Do not merge notes typeset in different style. */
  if (!ly_is_equal (nu->get_property ("style"),
		    nd->get_property ("style")))
    merge_possible = false;

  int upball_type = Note_head::get_balltype (nu);
  int dnball_type = Note_head::get_balltype (nd);

  /* Do not merge whole notes (or longer, like breve, longa, maxima).  */
  if (merge_possible && (upball_type <= 0 || dnball_type <= 0))
    merge_possible = false;

  if (merge_possible
      && Rhythmic_head::dot_count (nu) != Rhythmic_head::dot_count (nd)
      && !to_boolean (me->get_property ("merge-differently-dotted")))
    merge_possible = false;

  /* Can only merge different heads if merge-differently-headed is
     set. */
  if (merge_possible
      && upball_type != dnball_type
      && !to_boolean (me->get_property ("merge-differently-headed")))
    merge_possible = false;

  if (merge_possible
      && nu->get_property ("style") == ly_symbol2scm ("fa")
      && nd->get_property ("style") == ly_symbol2scm ("fa"))
    {
      Interval uphead_size = nu->extent (nu, Y_AXIS);
      Offset att =  Offset (0.0, -1.0);
      nu->set_property ("stem-attachment", ly_offset2scm (att));
      nu->set_property ("transparent", SCM_BOOL_T); 
    }
  
  /* Should never merge quarter and half notes, as this would make
     them indistinguishable.  */
  if (merge_possible
      && ((Rhythmic_head::duration_log (nu) == 1
	   && Rhythmic_head::duration_log (nd) == 2)
	  || (Rhythmic_head::duration_log (nu) == 2
	      && Rhythmic_head::duration_log (nd) == 1)))
    merge_possible = false;

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

  /* TODO: filter out the 'o's in this configuration, since they're no
     part in the collision.

     |
     x|o
     x|o
     x

  */

  bool close_half_collide = false;
  bool distant_half_collide = false;
  bool full_collide = false;

  int i = 0, j = 0;
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
      else if (ups[i] == dps[j])
	full_collide = true;
      else if (ups[i] > dps[0] && ups[i] < dps.top ())
	merge_possible = false;
      else if (dps[j] > ups[0] && dps[j] < ups.top ())
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

  full_collide = full_collide || (close_half_collide
				  && distant_half_collide);

  Drul_array<Real> center_note_shifts;
  center_note_shifts[LEFT] = 0.0;
  center_note_shifts[RIGHT] = 0.0;

  Real shift_amount = 1;

  bool touch = (ups[0] >= dps.top ());
  if (touch)
    shift_amount *= -1;

  /* For full collisions, the right hand head may obscure dots, so
     make sure the dotted heads go to the right.  */
  bool stem_to_stem = false;
  if (full_collide)
    if (Rhythmic_head::dot_count (nu) > Rhythmic_head::dot_count (nd))
      shift_amount = 1;
    else if (Rhythmic_head::dot_count (nu) < Rhythmic_head::dot_count (nd))
      stem_to_stem = true;

  if (merge_possible)
    {
      shift_amount = 0;

      /* If possible, don't wipe any heads. Else, wipe shortest head,
	 or head with smallest amount of dots.  Note: when merging
	 different heads, dots on the smaller one disappear. */
      Grob *wipe_ball = 0;
      Grob *dot_wipe_head = nu;

      if (upball_type == dnball_type)
	{
	  if (Rhythmic_head::dot_count (nd) < Rhythmic_head::dot_count (nu))
	    {
	      wipe_ball = nd;
	      dot_wipe_head = nd;
	    }
	  else if (Rhythmic_head::dot_count (nd) > Rhythmic_head::dot_count (nu))
	    {
	      dot_wipe_head = nu;
	      wipe_ball = nu;
	    }
	  else
	    dot_wipe_head = nu;
	}
      else if (dnball_type > upball_type)
	{
	  wipe_ball = nd;
	  dot_wipe_head = nd;
	}
      else if (dnball_type < upball_type)
	{
	  wipe_ball = nu;
	  dot_wipe_head = nu;
	}

      if (dot_wipe_head)
	{
	  if (Grob *d = unsmob_grob (dot_wipe_head->get_object ("dot")))
	    d->suicide ();
	}

      if (wipe_ball && wipe_ball->is_live ())
	{
	  wipe_ball->set_property ("transparent", SCM_BOOL_T);
	}
    }
  /* TODO: these numbers are magic; should devise a set of grob props
     to tune this behavior.  */
  else if (stem_to_stem)
    shift_amount = -abs (shift_amount) * 0.65;
  else if (close_half_collide && !touch)
    shift_amount *= 0.52;
  else if (distant_half_collide && !touch)
    shift_amount *= 0.4;
  else if (distant_half_collide || close_half_collide || full_collide)
    shift_amount *= 0.5;

  /* we're meshing.  */
  else if (Rhythmic_head::dot_count (nu) || Rhythmic_head::dot_count (nd))
    shift_amount *= 0.1;
  else
    shift_amount *= 0.17;

  /* For full or close half collisions, the right hand head may
     obscure dots.  Move dots to the right.  */
  if (abs (shift_amount) > 1e-6
      && Rhythmic_head::dot_count (nd) > Rhythmic_head::dot_count (nu)
      && (full_collide || close_half_collide))
    {
      Grob *d = unsmob_grob (nd->get_object ("dot"));
      Grob *parent = d->get_parent (X_AXIS);

      /*
	FIXME:

	|
	x . o
	|


	the . is put right of o which is erroneous o force-shifted
	far to the right.
      */
      if (Dot_column::has_interface (parent))
	Side_position_interface::add_support (parent, nu);
    }

  Direction d = UP;
  do
    {
      for (int i = 0; i < clash_groups[d].size (); i++)
	(*offsets)[d][i] += d * shift_amount;
    }
  while ((flip (&d)) != UP);
}


MAKE_SCHEME_CALLBACK(Note_collision_interface, calc_positioning_done, 1) 
SCM
Note_collision_interface::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);  
  Drul_array<Link_array<Grob> > cg = get_clash_groups (me);

  Direction d = UP;
  do
    {
      for (int i = cg[d].size(); i--; )
	{
	  /*
	    Trigger positioning
	   */
	  cg[d][i]->extent (me, X_AXIS);
	}
    }
  while (flip (&d) != UP);

  SCM autos (automatic_shift (me, cg));
  SCM hand (forced_shift (me));

  Real wid = 0.0;
  do
    {
      if (cg[d].size ())
	{
	  Grob *h = cg[d][0];
	  wid = Note_column::first_head (h)->extent (h, X_AXIS).length ();
	}
    }
  while (flip (&d) != UP);

  Link_array<Grob> done;
  Real left_most = 1e6;

  Array<Real> amounts;
  for (; scm_is_pair (hand); hand = scm_cdr (hand))
    {
      Grob *s = unsmob_grob (scm_caar (hand));
      Real amount = scm_to_double (scm_cdar (hand)) * wid;

      done.push (s);
      amounts.push (amount);
      if (amount < left_most)
	left_most = amount;
    }
  for (; scm_is_pair (autos); autos = scm_cdr (autos))
    {
      Grob *s = unsmob_grob (scm_caar (autos));
      Real amount = scm_to_double (scm_cdar (autos)) * wid;

      if (!done.find (s))
	{
	  done.push (s);
	  amounts.push (amount);
	  if (amount < left_most)
	    left_most = amount;
	}
    }

  for (int i = 0; i < amounts.size (); i++)
    done[i]->translate_axis (amounts[i] - left_most, X_AXIS);

  return SCM_BOOL_T;
}

Drul_array < Link_array<Grob> >
Note_collision_interface::get_clash_groups (Grob *me)
{
  Drul_array<Link_array<Grob> > clash_groups;

  extract_grob_set (me, "elements", elements);
  for (int i = 0; i < elements.size (); i++)
    {
      Grob *se = elements[i];
      if (Note_column::has_interface (se))
	clash_groups[Note_column::dir (se)].push (se);
    }

  Direction d = UP;
  do
    {
      Link_array<Grob> &clashes (clash_groups[d]);
      clashes.sort (Note_column::shift_compare);
    }
  while ((flip (&d)) != UP);

  return clash_groups;
}

/** This complicated routine moves note columns around horizontally to
    ensure that notes don't clash.

    This should be put into Scheme.
*/
SCM
Note_collision_interface::automatic_shift (Grob *me,
					   Drul_array < Link_array<Grob>
					   > clash_groups)
{
  Drul_array < Array<int> > shifts;
  SCM tups = SCM_EOL;

  Direction d = UP;
  do
    {
      Array<int> &shift (shifts[d]);
      Link_array<Grob> &clashes (clash_groups[d]);

      for (int i = 0; i < clashes.size (); i++)
	{
	  SCM sh
	    = clashes[i]->get_property ("horizontal-shift");

	  if (scm_is_number (sh))
	    shift.push (scm_to_int (sh));
	  else
	    shift.push (0);
	}

      for (int i = 1; i < shift.size (); i++)
	{
	  if (shift[i - 1] == shift[i])
	    {
	      clashes[0]->warning (_ ("ignoring too many clashing note columns"));
	      return tups;
	    }
	}
    }
  while ((flip (&d)) != UP);

  Drul_array<Array<Slice> > extents;
  Drul_array<Array<Real> > offsets;
  d = UP;
  do
    {
      for (int i = 0; i < clash_groups[d].size (); i++)
	{
	  Slice s (Note_column::head_positions_interval (clash_groups[d][i]));
	  s[LEFT]--;
	  s[RIGHT]++;
	  extents[d].push (s);
	  offsets[d].push (d * 0.5 * i);
	}
    }
  while ((flip (&d)) != UP);

  /*
    do horizontal shifts of each direction

    |
    x||
    x||
    x|
  */

  do
    {
      for (int i = 1; i < clash_groups[d].size (); i++)
	{
	  Slice prev = extents[d][i - 1];
	  prev.intersect (extents[d][i]);
	  if (prev.length () > 0
	      || (extents[-d].size () && d * (extents[d][i][-d] - extents[-d][0][d]) < 0))
	    for (int j = i; j < clash_groups[d].size (); j++)
	      offsets[d][j] += d * 0.5;
	}
    }
  while ((flip (&d)) != UP);

  /*
    Check if chords are meshing
  */

  check_meshing_chords (me, &offsets, extents, clash_groups);

  do
    {
      for (int i = 0; i < clash_groups[d].size (); i++)
	tups = scm_cons (scm_cons (clash_groups[d][i]->self_scm (),
				   scm_from_double (offsets[d][i])),
			 tups);
    }
  while (flip (&d) != UP);

  return tups;
}

SCM
Note_collision_interface::forced_shift (Grob *me)
{
  SCM tups = SCM_EOL;

  extract_grob_set (me, "elements", elements);
  for (int i = 0; i < elements.size (); i++)
    {
      Grob *se = elements[i];

      SCM force = se->get_property ("force-hshift");
      if (scm_is_number (force))
	{
	  tups = scm_cons (scm_cons (se->self_scm (), force),
			   tups);
	}
    }
  return tups;
}

void
Note_collision_interface::add_column (Grob *me, Grob *ncol)
{
  ncol->set_property ("X-offset", Grob::x_parent_positioning_proc);
  Axis_group_interface::add_element (me, ncol);
}

ADD_INTERFACE (Note_collision_interface, "note-collision-interface",
	       "An object that handles collisions between notes with different stem "
	       "directions and horizontal shifts. Most of the interesting properties "
	       "are to be set in @ref{note-column-interface}: these are "
	       "@code{force-hshift} and @code{horizontal-shift}.",

	       /* properties */
	       "merge-differently-dotted "
	       "merge-differently-headed "
	       "positioning-done");
