/*
  collision.cc -- implement Collision

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "note-collision.hh"

#include "axis-group-interface.hh"
#include "dot-column.hh"
#include "international.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "item.hh"
#include "rhythmic-head.hh"
#include "staff-symbol-referencer.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "warn.hh"


void
check_meshing_chords (Grob *me,
		      Drul_array<vector<Real> > *offsets,
		      Drul_array<vector<Slice> > const &extents,
		      Drul_array<vector<Grob*> > const &clash_groups)

{
  if (!extents[UP].size () || ! extents[DOWN].size ())
    return;

  Grob *cu = clash_groups[UP][0];
  Grob *cd = clash_groups[DOWN][0];

  /* Every note column should have a stem, but avoid a crash. */
  if (!Note_column::get_stem (cu) || !Note_column::get_stem (cd))
    return;

  Drul_array<Grob*> stems (Note_column::get_stem (cd),
			   Note_column::get_stem (cu));
  
  Grob *nu = Note_column::first_head (cu);
  Grob *nd = Note_column::first_head (cd);

  vector<int> ups = Stem::note_head_positions (Note_column::get_stem (cu));
  vector<int> dps = Stem::note_head_positions (Note_column::get_stem (cd));

  /* Too far apart to collide.  */
  if (ups[0] > dps.back () + 1)
    return;

  /* Merge heads if the notes lie the same line, or if the "stem-up-note" is
     above the "stem-down-note". */
  bool merge_possible = (ups[0] >= dps[0]) && (ups.back () >= dps.back ());

  /* Do not merge notes typeset in different style. */
  if (!ly_is_equal (nu->get_property ("style"),
		    nd->get_property ("style")))
    merge_possible = false;

  int upball_type = Rhythmic_head::duration_log (nu);
  int dnball_type = Rhythmic_head::duration_log (nd);

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
      && ((Stem::duration_log (stems[UP]) == 1
	   && Stem::duration_log (stems[DOWN]) == 2)
	  || (Stem::duration_log (stems[UP]) == 2
	      && Stem::duration_log (stems[DOWN]) == 1)))
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

  for (vsize i = 0, j = 0; i < ups.size () && j < dps.size (); )
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
      else if (ups[i] > dps[0] && ups[i] < dps.back ())
	merge_possible = false;
      else if (dps[j] > ups[0] && dps[j] < ups.back ())
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

  bool touch = (ups[0] >= dps.back ());
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

  /*
    
  */
  if (full_collide
      && dnball_type * upball_type == 0)
    {
      if (upball_type == 0 && dnball_type == 1)
	shift_amount *= 1.25;
      else if (upball_type == 0 && dnball_type == 2)
	shift_amount *= 1.35;
      else if (dnball_type == 0 && upball_type == 1)
	shift_amount *= 0.7;
      else if (dnball_type == 0 && upball_type == 2)
	shift_amount *= 0.75;
    }
  
  /*
   * Fix issue #44:
   *
   * Dots from left note head collide with right note head. Only occurs
   * with a close half collide, if the left note head is between
   * lines and the right note head is on a line, and if right note head
   * hasn't got any dots.
   */
  if (close_half_collide
      && Rhythmic_head::dot_count (nu)
      && !Rhythmic_head::dot_count (nd))
    {
      Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
      if (!Staff_symbol_referencer::on_line (staff, ups[0]))
	{
	  Grob *d = unsmob_grob (nu->get_object ("dot"));
	  Grob *parent = d->get_parent (X_AXIS);
	  if (Dot_column::has_interface (parent))
	    Side_position_interface::add_support (parent, nd);
	}
    }

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
	{
	  Grob *stem = unsmob_grob (nu->get_object ("stem"));
	  extract_grob_set (stem, "note-heads", heads);
	  for (vsize i = 0; i < heads.size (); i++)
	    Side_position_interface::add_support (parent, heads[i]);
	}
    }

  Direction d = UP;
  do
    {
      for (vsize i = 0; i < clash_groups[d].size (); i++)
	(*offsets)[d][i] += d * shift_amount;
    }
  while ((flip (&d)) != UP);
}


MAKE_SCHEME_CALLBACK (Note_collision_interface, calc_positioning_done, 1) 
SCM
Note_collision_interface::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  me->set_property ("positioning-done", SCM_BOOL_T);
  
  Drul_array<vector<Grob*> > cg = get_clash_groups (me);

  Direction d = UP;
  do
    {
      for (vsize i = cg[d].size (); i--; )
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
	  Grob *fh = Note_column::first_head (h);
	  if (fh)
	    wid = fh->extent (h, X_AXIS).length ();
	}
    }
  while (flip (&d) != UP);

  vector<Grob*> done;
  Real left_most = 1e6;

  vector<Real> amounts;
  for (; scm_is_pair (hand); hand = scm_cdr (hand))
    {
      Grob *s = unsmob_grob (scm_caar (hand));
      Real amount = scm_to_double (scm_cdar (hand)) * wid;

      done.push_back (s);
      amounts.push_back (amount);
      if (amount < left_most)
	left_most = amount;
    }
  for (; scm_is_pair (autos); autos = scm_cdr (autos))
    {
      Grob *s = unsmob_grob (scm_caar (autos));
      Real amount = scm_to_double (scm_cdar (autos)) * wid;

      vsize x = find (done, s) - done.begin ();
      if (x == VPOS || x >= done.size ())
	{
	  done.push_back (s);
	  amounts.push_back (amount);
	  if (amount < left_most)
	    left_most = amount;
	}
    }

  for (vsize i = 0; i < amounts.size (); i++)
    done[i]->translate_axis (amounts[i] - left_most, X_AXIS);

  return SCM_BOOL_T;
}

Drul_array < vector<Grob*> >
Note_collision_interface::get_clash_groups (Grob *me)
{
  Drul_array<vector<Grob*> > clash_groups;

  extract_grob_set (me, "elements", elements);
  for (vsize i = 0; i < elements.size (); i++)
    {
      Grob *se = elements[i];
      if (Note_column::has_interface (se))
	{
	  if (!Note_column::dir (se))
	    {
	      se->programming_error ("note-column has no direction");
	    }
	  else
	    clash_groups[Note_column::dir (se)].push_back (se);
	}
    }

  Direction d = UP;
  do
    {
      vector<Grob*> &clashes (clash_groups[d]);
      vector_sort (clashes, Note_column::shift_less);
    }
  while ((flip (&d)) != UP);

  return clash_groups;
}

/** This complicated routine moves note columns around horizontally to
    ensure that notes don't clash.

*/
SCM
Note_collision_interface::automatic_shift (Grob *me,
					   Drul_array<vector<Grob*> > clash_groups)
{
  Drul_array < vector<int> > shifts;
  SCM tups = SCM_EOL;

  Direction d = UP;
  do
    {
      vector<int> &shift (shifts[d]);
      vector<Grob*> &clashes (clash_groups[d]);

      for (vsize i = 0; i < clashes.size (); i++)
	{
	  SCM sh
	    = clashes[i]->get_property ("horizontal-shift");

	  if (scm_is_number (sh))
	    shift.push_back (scm_to_int (sh));
	  else
	    shift.push_back (0);
	}

      for (vsize i = 1; i < shift.size (); i++)
	{
	  if (shift[i - 1] == shift[i])
	    {
	      clashes[0]->warning (_ ("ignoring too many clashing note columns"));
	      return tups;
	    }
	}
    }
  while ((flip (&d)) != UP);

  Drul_array<vector<Slice> > extents;
  Drul_array<vector<Real> > offsets;
  d = UP;
  do
    {
      for (vsize i = 0; i < clash_groups[d].size (); i++)
	{
	  Slice s (Note_column::head_positions_interval (clash_groups[d][i]));
	  s[LEFT]--;
	  s[RIGHT]++;
	  extents[d].push_back (s);
	  offsets[d].push_back (d * 0.5 * i);
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
      for (vsize i = 1; i < clash_groups[d].size (); i++)
	{
	  Slice prev = extents[d][i - 1];
	  prev.intersect (extents[d][i]);
	  if (prev.length () > 0
	      || (extents[-d].size () && d * (extents[d][i][-d] - extents[-d][0][d]) < 0))
	    for (vsize j = i; j < clash_groups[d].size (); j++)
	      offsets[d][j] += d * 0.5;
	}
    }
  while ((flip (&d)) != UP);


  /*
    see input/regression/dot-up-voice-collision.ly
   */
  for (vsize i = 0; i < clash_groups[UP].size (); i++)
    {
      Grob *g = clash_groups[UP][i];
      Grob *dc = Note_column::dot_column (g);
      
      if (dc)
	for (vsize j = i + 1;  j < clash_groups[UP].size (); j++)
	  {
	    Grob *stem = Note_column::get_stem (clash_groups[UP][j]);
	    Side_position_interface::add_support (dc, stem);
	  }
    }
  
  /*
    Check if chords are meshing
  */

  check_meshing_chords (me, &offsets, extents, clash_groups);

  do
    {
      for (vsize i = 0; i < clash_groups[d].size (); i++)
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
  for (vsize i = 0; i < elements.size (); i++)
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

ADD_INTERFACE (Note_collision_interface,
	       "An object that handles collisions between notes with different stem "
	       "directions and horizontal shifts. Most of the interesting properties "
	       "are to be set in @ref{note-column-interface}: these are "
	       "@code{force-hshift} and @code{horizontal-shift}.",

	       /* properties */
	       "ignore-collision "
	       "merge-differently-dotted "
	       "merge-differently-headed "
	       "positioning-done ");
