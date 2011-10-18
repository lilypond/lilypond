/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

  TODO: This is way too hairy

  TODO: fix naming.

  Stem-end, chord-start, etc. is all confusing naming.

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
  Note that several internal functions have a calc_beam bool argument.
  This argument means: "If set, acknowledge the fact that there is a beam
  and deal with it.  If not, give me the measurements as if there is no beam."
  Most pure functions are called WITHOUT calc_beam, whereas non-pure functions
  are called WITH calc_beam.

  The only exception to this is ::pure_height, which calls internal_pure_height
  with "true" for calc_beam in order to trigger the calculations of other
  pure heights in case there is a beam.  It passes false, however, to
  internal_height and internal_pure_height for all subsequent iterations.
*/

#include "stem.hh"
#include "spanner.hh"

#include <cmath>                // rint
using namespace std;

#include "beam.hh"
#include "directional-element-interface.hh"
#include "dot-column.hh"
#include "font-interface.hh"
#include "international.hh"
#include "lookup.hh"
#include "misc.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "rest.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stem-tremolo.hh"
#include "warn.hh"

void
Stem::set_beaming (Grob *me, int beam_count, Direction d)
{
  SCM pair = me->get_property ("beaming");

  if (!scm_is_pair (pair))
    {
      pair = scm_cons (SCM_EOL, SCM_EOL);
      me->set_property ("beaming", pair);
    }

  SCM lst = index_get_cell (pair, d);
  if (beam_count)
    for (int i = 0; i < beam_count; i++)
      lst = scm_cons (scm_from_int (i), lst);
  else
    lst = SCM_BOOL_F;

  index_set_cell (pair, d, lst);
}

int
Stem::get_beaming (Grob *me, Direction d)
{
  SCM pair = me->get_property ("beaming");
  if (!scm_is_pair (pair))
    return 0;

  SCM lst = index_get_cell (pair, d);

  int len = scm_ilength (lst);
  return max (len, 0);
}

Interval
Stem::head_positions (Grob *me)
{
  if (head_count (me))
    {
      Drul_array<Grob *> e (extremal_heads (me));
      return Interval (Staff_symbol_referencer::get_position (e[DOWN]),
                       Staff_symbol_referencer::get_position (e[UP]));
    }
  return Interval ();
}

Real
Stem::chord_start_y (Grob *me)
{
  Interval hp = head_positions (me);
  if (!hp.is_empty ())
    return hp[get_grob_direction (me)] * Staff_symbol_referencer::staff_space (me)
           * 0.5;
  return 0;
}

void
Stem::set_stem_positions (Grob *me, Real se)
{
  // todo: margins
  Direction d = get_grob_direction (me);

  Grob *beam = unsmob_grob (me->get_object ("beam"));
  if (d && d * head_positions (me)[get_grob_direction (me)] >= se * d)
    me->warning (_ ("weird stem size, check for narrow beams"));

  Interval height = me->pure_height (me, 0, INT_MAX);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real half_space =  staff_space * 0.5;

  height[d] = se * half_space + beam_end_corrective (me);

  Real stemlet_length = robust_scm2double (me->get_property ("stemlet-length"),
                                           0.0);
  bool stemlet = stemlet_length > 0.0;

  Grob *lh = get_reference_head (me);

  if (!lh)
    {
      if (stemlet && beam)
        {
          Real beam_translation = Beam::get_beam_translation (beam);
          Real beam_thickness = Beam::get_beam_thickness (beam);
          int beam_count = beam_multiplicity (me).length () + 1;

          height[-d] = (height[d] - d
                        * (0.5 * beam_thickness
                        + beam_translation * max (0, (beam_count - 1))
                        + stemlet_length));
        }
      else if (!stemlet && beam)
        height[-d] = height[d];
      else if (stemlet && !beam)
        me->programming_error ("Can't have a stemlet without a beam.");
    }

  me->set_property ("stem-begin-position", scm_from_double (height[-d] * 2 / staff_space));
  me->set_property ("length", scm_from_double (height.length () * 2 / staff_space));
}

/* Note head that determines hshift for upstems
   WARNING: triggers direction  */
Grob *
Stem::support_head (Grob *me)
{
  extract_grob_set (me, "note-heads", heads);
  if (heads.size () == 1)
    return heads[0];

  return first_head (me);
}

int
Stem::head_count (Grob *me)
{
  return Pointer_group_interface::count (me, ly_symbol2scm ("note-heads"));
}

/* The note head which forms one end of the stem.
   WARNING: triggers direction  */
Grob *
Stem::first_head (Grob *me)
{
  Direction d = get_grob_direction (me);
  if (d)
    return extremal_heads (me)[-d];
  return 0;
}

/* The note head opposite to the first head.  */
Grob *
Stem::last_head (Grob *me)
{
  Direction d = get_grob_direction (me);
  if (d)
    return extremal_heads (me)[d];
  return 0;
}

/*
  START is part where stem reaches `last' head.

  This function returns a drul with (bottom-head, top-head).
*/
Drul_array<Grob *>
Stem::extremal_heads (Grob *me)
{
  const int inf = INT_MAX;
  Drul_array<int> extpos;
  extpos[DOWN] = inf;
  extpos[UP] = -inf;

  Drul_array<Grob *> exthead (0, 0);
  extract_grob_set (me, "note-heads", heads);

  for (vsize i = heads.size (); i--;)
    {
      Grob *n = heads[i];
      int p = Staff_symbol_referencer::get_rounded_position (n);

      Direction d = LEFT;
      do
        {
          if (d * p > d * extpos[d])
            {
              exthead[d] = n;
              extpos[d] = p;
            }
        }
      while (flip (&d) != DOWN);
    }
  return exthead;
}

/* The positions, in ascending order.  */
vector<int>
Stem::note_head_positions (Grob *me)
{
  vector<int> ps;
  extract_grob_set (me, "note-heads", heads);

  for (vsize i = heads.size (); i--;)
    {
      Grob *n = heads[i];
      int p = Staff_symbol_referencer::get_rounded_position (n);

      ps.push_back (p);
    }

  vector_sort (ps, less<int> ());
  return ps;
}

void
Stem::add_head (Grob *me, Grob *n)
{
  n->set_object ("stem", me->self_scm ());

  if (Note_head::has_interface (n))
    Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-heads"), n);
  else if (Rest::has_interface (n))
    Pointer_group_interface::add_grob (me, ly_symbol2scm ("rests"), n);
}

bool
Stem::is_invisible (Grob *me)
{
  return !is_normal_stem (me)
         && (robust_scm2double (me->get_property ("stemlet-length"),
                                0.0) == 0.0);
}

bool
Stem::is_normal_stem (Grob *me)
{
  return head_count (me) && scm_to_int (me->get_property ("duration-log")) >= 1;
}

MAKE_SCHEME_CALLBACK (Stem, pure_height, 3)
SCM
Stem::pure_height (SCM smob,
                   SCM /* start */,
                   SCM /* end */)
{
  Grob *me = unsmob_grob (smob);
  return ly_interval2scm (internal_pure_height (me, true));
}

Interval
Stem::internal_pure_height (Grob *me, bool calc_beam)
{
  if (!is_normal_stem (me))
    return Interval (0.0, 0.0);

  Grob *beam = unsmob_grob (me->get_object ("beam"));

  Interval iv = internal_height (me, false);

  if (!beam)
    return iv;
  if (!to_boolean (me->get_property ("cross-staff")) && calc_beam)
    {
      Interval overshoot;
      Direction dir = get_grob_direction (me);
      Direction d = DOWN;
      do
        overshoot[d] = d == dir ? dir * infinity_f : iv[d];
      while (flip (&d) != DOWN);

      vector<Interval> heights;
      vector<Grob *> my_stems;
      extract_grob_set (beam, "normal-stems", normal_stems);
      for (vsize i = 0; i < normal_stems.size (); i++)
        if (normal_stems[i] != me && get_grob_direction (normal_stems[i]) == dir)
          {
            heights.push_back (Stem::internal_pure_height (normal_stems[i], false));
            my_stems.push_back (normal_stems[i]);
            iv.unite (heights.back ());
          }
      for (vsize i = 0; i < my_stems.size (); i++)
        cache_pure_height (my_stems[i], iv, heights[i]);
      iv.intersect (overshoot);
    }

  return iv;
}

void
Stem::cache_pure_height (Grob *me, Interval iv, Interval my_iv)
{
  Interval overshoot;
  Direction dir = get_grob_direction (me);
  Direction d = DOWN;
  do
    overshoot[d] = d == dir ? dir * infinity_f : my_iv[d];
  while (flip (&d) != DOWN);

  iv.intersect (overshoot);
  dynamic_cast<Item *> (me)->cache_pure_height (iv);
}

MAKE_SCHEME_CALLBACK (Stem, calc_stem_end_position, 1)
SCM
Stem::calc_stem_end_position (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return scm_from_double (internal_calc_stem_end_position (me, true));
}

MAKE_SCHEME_CALLBACK (Stem, pure_calc_stem_end_position, 3)
SCM
Stem::pure_calc_stem_end_position (SCM smob,
                                   SCM, /* start */
                                   SCM /* end */)
{
  Grob *me = unsmob_grob (smob);
  return scm_from_double (internal_calc_stem_end_position (me, false));
}

Real
Stem::internal_calc_stem_end_position (Grob *me, bool calc_beam)
{
  if (!head_count (me))
    return 0.0;

  Grob *beam = get_beam (me);
  Real ss = Staff_symbol_referencer::staff_space (me);
  Direction dir = get_grob_direction (me);

  if (beam && calc_beam)
    {
      (void) beam->get_property ("quantized-positions");
      return robust_scm2double (me->get_property ("length"), 0.0)
             + dir * robust_scm2double (me->get_property ("stem-begin-position"), 0.0);
    }

  vector<Real> a;

  /* WARNING: IN HALF SPACES */
  SCM details = me->get_property ("details");
  int durlog = duration_log (me);

  Real staff_rad = Staff_symbol_referencer::staff_radius (me);
  Real length = 7;
  SCM s = ly_assoc_get (ly_symbol2scm ("lengths"), details, SCM_EOL);
  if (scm_is_pair (s))
    length = 2 * scm_to_double (robust_list_ref (durlog - 2, s));


  /* Stems in unnatural (forced) direction should be shortened,
     according to [Roush & Gourlay] */
  Interval hp = head_positions (me);
  if (dir && dir * hp[dir] >= 0)
    {
      SCM sshorten = ly_assoc_get (ly_symbol2scm ("stem-shorten"), details, SCM_EOL);
      SCM scm_shorten = scm_is_pair (sshorten)
                        ? robust_list_ref (max (duration_log (me) - 2, 0), sshorten) : SCM_EOL;
      Real shorten_property = 2 * robust_scm2double (scm_shorten, 0);
      /*  change in length between full-size and shortened stems is executed gradually.
          "transition area" = stems between full-sized and fully-shortened.
          */
      Real quarter_stem_length = 2 * scm_to_double (robust_list_ref (0, s));
      /*  shortening_step = difference in length between consecutive stem lengths
          in transition area. The bigger the difference between full-sized
          and shortened stems, the bigger shortening_step is.
          (but not greater than 1/2 and not smaller than 1/4).
          value 6 is heuristic; it determines the suggested transition slope steepnesas.
          */
      Real shortening_step = min (max (0.25, (shorten_property / 6)), 0.5);
      /*  Shortening of unflagged stems should begin on the first stem that sticks
          more than 1 staffspace (2 units) out of the staff.
          Shortening of flagged stems begins in the same moment as unflagged ones,
          but not earlier than on the middle line note.
          */
      Real which_step = (min (1.0, quarter_stem_length - (2 * staff_rad) - 2.0)) + abs (hp[dir]);
      Real shorten = min (max (0.0, (shortening_step * which_step)), shorten_property);

      length -= shorten;
    }

  length *= robust_scm2double (me->get_property ("length-fraction"), 1.0);

  /* Tremolo stuff.  */
  Grob *t_flag = unsmob_grob (me->get_object ("tremolo-flag"));
  if (t_flag && (!unsmob_grob (me->get_object ("beam")) || !calc_beam))
    {
      /* Crude hack: add extra space if tremolo flag is there.

      We can't do this for the beam, since we get into a loop
      (Stem_tremolo::raw_stencil () looks at the beam.) --hwn  */

      Real minlen = 1.0
                    + 2 * Stem_tremolo::vertical_length (t_flag) / ss;

      /* We don't want to add the whole extent of the flag because the trem
         and the flag can overlap partly. beam_translation gives a good
         approximation */
      if (durlog >= 3)
        {
          Real beam_trans = Stem_tremolo::get_beam_translation (t_flag);
          /* the obvious choice is (durlog - 2) here, but we need a bit more space. */
          minlen += 2 * (durlog - 1.5) * beam_trans;

          /* up-stems need even a little more space to avoid collisions. This
             needs to be in sync with the tremolo positioning code in
             Stem_tremolo::print */
          if (dir == UP)
            minlen += beam_trans;
        }
      length = max (length, minlen + 1.0);
    }

  Real stem_end = dir ? hp[dir] + dir * length : 0;

  /* TODO: change name  to extend-stems to staff/center/'()  */
  bool no_extend = to_boolean (me->get_property ("no-stem-extend"));
  if (!no_extend && dir * stem_end < 0)
    stem_end = 0.0;

  return stem_end;
}

/* The log of the duration (Number of hooks on the flag minus two)  */
int
Stem::duration_log (Grob *me)
{
  SCM s = me->get_property ("duration-log");
  return (scm_is_number (s)) ? scm_to_int (s) : 2;
}

MAKE_SCHEME_CALLBACK (Stem, calc_positioning_done, 1);
SCM
Stem::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (!head_count (me))
    return SCM_BOOL_T;

  me->set_property ("positioning-done", SCM_BOOL_T);

  extract_grob_set (me, "note-heads", ro_heads);
  vector<Grob *> heads (ro_heads);
  vector_sort (heads, position_less);
  Direction dir = get_grob_direction (me);

  if (dir < 0)
    reverse (heads);

  Real thick = thickness (me);

  Grob *hed = support_head (me);
  if (!dir)
    {
      programming_error ("Stem dir must be up or down.");
      dir = UP;
      set_grob_direction (me, dir);
    }

  bool is_harmonic_centered = false;
  for (vsize i = 0; i < heads.size (); i++)
    is_harmonic_centered = is_harmonic_centered
                           || heads[i]->get_property ("style") == ly_symbol2scm ("harmonic");
  is_harmonic_centered = is_harmonic_centered && is_invisible (me);

  Real w = hed->extent (hed, X_AXIS)[dir];
  for (vsize i = 0; i < heads.size (); i++)
    {
      Real amount = w - heads[i]->extent (heads[i], X_AXIS)[dir];

      if (is_harmonic_centered)
        amount
          = hed->extent (hed, X_AXIS).linear_combination (CENTER)
            - heads[i]->extent (heads[i], X_AXIS).linear_combination (CENTER);

      heads[i]->translate_axis (amount, X_AXIS);
    }
  bool parity = true;
  Real lastpos = Real (Staff_symbol_referencer::get_position (heads[0]));
  for (vsize i = 1; i < heads.size (); i++)
    {
      Real p = Staff_symbol_referencer::get_position (heads[i]);
      Real dy = fabs (lastpos - p);

      /*
        dy should always be 0.5, 0.0, 1.0, but provide safety margin
        for rounding errors.
      */
      if (dy < 1.1)
        {
          if (parity)
            {
              Real ell = heads[i]->extent (heads[i], X_AXIS).length ();

              Direction d = get_grob_direction (me);
              /*
                Reversed head should be shifted ell-thickness, but this
                looks too crowded, so we only shift ell-0.5*thickness.

                This leads to assymetry: Normal heads overlap the
                stem 100% whereas reversed heads only overlaps the
                stem 50%
              */

              Real reverse_overlap = 0.5;
              heads[i]->translate_axis ((ell - thick * reverse_overlap) * d,
                                        X_AXIS);

              if (is_invisible (me))
                heads[i]->translate_axis (-thick * (2 - reverse_overlap) * d,
                                          X_AXIS);

              /* TODO:

              For some cases we should kern some more: when the
              distance between the next or prev note is too large, we'd
              get large white gaps, eg.

              |
              X|
              |X  <- kern this.
              |
              X

              */
            }
          parity = !parity;
        }
      else
        parity = true;

      lastpos = int (p);
    }

  return SCM_BOOL_T;
}

MAKE_SCHEME_CALLBACK (Stem, calc_direction, 1);
SCM
Stem::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Direction dir = CENTER;
  if (Grob *beam = unsmob_grob (me->get_object ("beam")))
    {
      SCM ignore_me = beam->get_property ("direction");
      (void) ignore_me;
      dir = get_grob_direction (me);
    }
  else
    {
      SCM dd = me->get_property ("default-direction");
      dir = to_dir (dd);
      if (!dir)
        return me->get_property ("neutral-direction");
    }

  return scm_from_int (dir);
}

MAKE_SCHEME_CALLBACK (Stem, calc_default_direction, 1);
SCM
Stem::calc_default_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Direction dir = CENTER;
  int staff_center = 0;
  Interval hp = head_positions (me);
  if (!hp.is_empty ())
    {
      int udistance = (int) (UP * hp[UP] - staff_center);
      int ddistance = (int) (DOWN * hp[DOWN] - staff_center);

      dir = Direction (sign (ddistance - udistance));
    }

  return scm_from_int (dir);
}

// note - height property necessary to trigger quantized beam positions
// otherwise, we could just use Grob::stencil_height_proc
MAKE_SCHEME_CALLBACK (Stem, height, 1);
SCM
Stem::height (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return ly_interval2scm (internal_height (me, true));
}

Grob*
Stem::get_reference_head (Grob *me)
{
  return to_boolean (me->get_property ("avoid-note-head"))
         ? last_head (me)
         : first_head (me);
}

Real
Stem::beam_end_corrective (Grob *me)
{
  Grob *beam = unsmob_grob (me->get_object ("beam"));
  Direction dir = get_grob_direction (me);
  if (beam)
    {
      if (dir == CENTER)
        {
          programming_error ("no stem direction");
          dir = UP;
        }
      return dir * Beam::get_beam_thickness (beam) * 0.5;
    }
  return 0.0;
}

Interval
Stem::internal_height (Grob *me, bool calc_beam)
{
  Grob *beam = get_beam (me);
  if (!is_valid_stem (me) && ! beam)
    return Interval ();

  Direction dir = get_grob_direction (me);

  if (beam && calc_beam)
    {
      /* trigger set-stem-lengths. */
      (void) beam->get_property ("quantized-positions");
    }

  Real y1 = robust_scm2double ((calc_beam
                                ? me->get_property ("stem-begin-position")
                                : me->get_pure_property ("stem-begin-position", 0, INT_MAX)),
                               0.0);

  Real y2 = dir * robust_scm2double ((calc_beam
                                     ? me->get_property ("length")
                                     : me->get_pure_property ("length", 0, INT_MAX)),
                                      0.0)
                + y1;

  Real half_space = Staff_symbol_referencer::staff_space (me) * 0.5;

  Interval stem_y  = Interval (min (y1, y2), max (y2, y1)) * half_space;

  return stem_y;
}

MAKE_SCHEME_CALLBACK (Stem, width, 1);
SCM
Stem::width (SCM e)
{
  Grob *me = unsmob_grob (e);

  Interval r;

  if (is_invisible (me))
    r.set_empty ();
  else
    {
      r = Interval (-1, 1);
      r *= thickness (me) / 2;
    }

  return ly_interval2scm (r);
}

Real
Stem::thickness (Grob *me)
{
  return scm_to_double (me->get_property ("thickness"))
         * Staff_symbol_referencer::line_thickness (me);
}

MAKE_SCHEME_CALLBACK (Stem, calc_stem_begin_position, 1);
SCM
Stem::calc_stem_begin_position (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return scm_from_double (internal_calc_stem_begin_position (me, true));
}

MAKE_SCHEME_CALLBACK (Stem, pure_calc_stem_begin_position, 3);
SCM
Stem::pure_calc_stem_begin_position (SCM smob,
                                     SCM, /* start */
                                     SCM /* end */)
{
  Grob *me = unsmob_grob (smob);
  return scm_from_double (internal_calc_stem_begin_position (me, false));
}

Real
Stem::internal_calc_stem_begin_position (Grob *me, bool calc_beam)
{
  Grob *beam = get_beam (me);
  Real ss = Staff_symbol_referencer::staff_space (me);
  if (beam && calc_beam)
    {
      (void) beam->get_property ("quantized-positions");
      return robust_scm2double (me->get_property ("stem-begin-position"), 0.0);
    }

  Direction d = get_grob_direction (me);
  Grob *lh = get_reference_head (me);

  if (!lh)
    return 0.0;

  Real pos = Staff_symbol_referencer::get_position (lh);

  if (Grob *head = support_head (me))
    {
      Interval head_height = head->extent (head, Y_AXIS);
      Real y_attach = Note_head::stem_attachment_coordinate (head, Y_AXIS);

      y_attach = head_height.linear_combination (y_attach);
      pos += d * y_attach * 2 / ss;
    }

  return pos;
}

bool
Stem::is_valid_stem (Grob *me)
{
  /* TODO: make the stem start a direction ?
     This is required to avoid stems passing in tablature chords.  */
  Grob *lh = get_reference_head (me);
  Grob *beam = unsmob_grob (me->get_object ("beam"));

  if (!lh && !beam)
    return false;

  if (lh && robust_scm2int (lh->get_property ("duration-log"), 0) < 1)
    return false;

  if (is_invisible (me))
    return false;

  return true;
}

MAKE_SCHEME_CALLBACK (Stem, print, 1);
SCM
Stem::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (!is_valid_stem (me))
    return SCM_EOL;

  Direction dir = get_grob_direction (me);
  Real y1 = robust_scm2double (me->get_property ("stem-begin-position"), 0.0);
  Real y2 = dir * robust_scm2double (me->get_property ("length"), 0.0) + y1;

  Real half_space = Staff_symbol_referencer::staff_space (me) * 0.5;

  Interval stem_y  = Interval (min (y1, y2), max (y2, y1)) * half_space;

  stem_y[dir] -= beam_end_corrective (me);

  // URG
  Real stem_width = thickness (me);
  Real blot
    = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  Box b = Box (Interval (-stem_width / 2, stem_width / 2),
               stem_y);

  Stencil mol;
  Stencil ss = Lookup::round_filled_box (b, blot);
  mol.add_stencil (ss);

  return mol.smobbed_copy ();
}

/*
  move the stem to right of the notehead if it is up.
*/
MAKE_SCHEME_CALLBACK (Stem, offset_callback, 1);
SCM
Stem::offset_callback (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  extract_grob_set (me, "rests", rests);
  if (rests.size ())
    {
      Grob *rest = rests.back ();
      Real r = rest->extent (rest, X_AXIS).center ();
      return scm_from_double (r);
    }

  if (Grob *f = first_head (me))
    {
      Interval head_wid = f->extent (f, X_AXIS);
      Real attach = 0.0;

      if (is_invisible (me))
        attach = 0.0;
      else
        attach = Note_head::stem_attachment_coordinate (f, X_AXIS);

      Direction d = get_grob_direction (me);
      Real real_attach = head_wid.linear_combination (d * attach);
      Real r = real_attach;

      /* If not centered: correct for stem thickness.  */
      string style = robust_symbol2string (f->get_property ("style"), "default");
      if (attach && style != "mensural"
                 && style != "neomensural"
                 && style != "petrucci")
        {
          Real rule_thick = thickness (me);
          r += -d * rule_thick * 0.5;
        }
      return scm_from_double (r);
    }

  programming_error ("Weird stem.");
  return scm_from_double (0.0);
}

Spanner *
Stem::get_beam (Grob *me)
{
  SCM b = me->get_object ("beam");
  return dynamic_cast<Spanner *> (unsmob_grob (b));
}

Stem_info
Stem::get_stem_info (Grob *me)
{
  Stem_info si;
  si.dir_ = get_grob_direction (me);

  SCM scm_info = me->get_property ("stem-info");
  si.ideal_y_ = scm_to_double (scm_car (scm_info));
  si.shortest_y_ = scm_to_double (scm_cadr (scm_info));
  return si;
}

MAKE_SCHEME_CALLBACK (Stem, calc_stem_info, 1);
SCM
Stem::calc_stem_info (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Direction my_dir = get_grob_direction (me);

  if (!my_dir)
    {
      programming_error ("no stem dir set");
      my_dir = UP;
    }

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Grob *beam = get_beam (me);

  if (beam)
    {
      (void) beam->get_property ("beaming");
    }

  Real beam_translation = Beam::get_beam_translation (beam);
  Real beam_thickness = Beam::get_beam_thickness (beam);
  int beam_count = Beam::get_direction_beam_count (beam, my_dir);
  Real length_fraction
    = robust_scm2double (me->get_property ("length-fraction"), 1.0);

  /* Simple standard stem length */
  SCM details = me->get_property ("details");
  SCM lengths = ly_assoc_get (ly_symbol2scm ("beamed-lengths"), details, SCM_EOL);

  Real ideal_length
    = (scm_is_pair (lengths)
       ? (scm_to_double (robust_list_ref (beam_count - 1, lengths))
          * staff_space
          * length_fraction
          /*
            stem only extends to center of beam
          */
          - 0.5 * beam_thickness)
       : 0.0);

  /* Condition: sane minimum free stem length (chord to beams) */
  lengths = ly_assoc_get (ly_symbol2scm ("beamed-minimum-free-lengths"),
                          details, SCM_EOL);

  Real ideal_minimum_free
    = (scm_is_pair (lengths)
       ? (scm_to_double (robust_list_ref (beam_count - 1, lengths))
          * staff_space
          * length_fraction)
       : 0.0);

  Real height_of_my_trem = 0.0;
  Grob *trem = unsmob_grob (me->get_object ("tremolo-flag"));
  if (trem)
    {
      height_of_my_trem
        = Stem_tremolo::vertical_length (trem)
          /* hack a bit of space around the trem. */
          + beam_translation;
    }

  /* UGH
     It seems that also for ideal minimum length, we must use
     the maximum beam count (for this direction):

     \score { \relative c'' { a8[ a32] } }

     must be horizontal. */
  Real height_of_my_beams = beam_thickness
                            + (beam_count - 1) * beam_translation;

  Real ideal_minimum_length = ideal_minimum_free
                              + height_of_my_beams
                              + height_of_my_trem
                              /* stem only extends to center of beam */
                              - 0.5 * beam_thickness;

  ideal_length = max (ideal_length, ideal_minimum_length);

  /* Convert to Y position, calculate for dir == UP */
  Real note_start
    =     /* staff positions */
      head_positions (me)[my_dir] * 0.5
      * my_dir * staff_space;
  Real ideal_y = note_start + ideal_length;

  /* Conditions for Y position */

  /* Lowest beam of (UP) beam must never be lower than second staffline

  Reference?

  Although this (additional) rule is probably correct,
  I expect that highest beam (UP) should also never be lower
  than middle staffline, just as normal stems.

  Reference?

  Obviously not for grace beams.

  Also, not for knees.  Seems to be a good thing. */
  bool no_extend = to_boolean (me->get_property ("no-stem-extend"));
  bool is_knee = Beam::is_knee (beam);
  if (!no_extend && !is_knee)
    {
      /* Highest beam of (UP) beam must never be lower than middle
         staffline */
      ideal_y = max (ideal_y, 0.0);
      /* Lowest beam of (UP) beam must never be lower than second staffline */
      ideal_y = max (ideal_y, (-staff_space
                               - beam_thickness + height_of_my_beams));
    }

  ideal_y -= robust_scm2double (beam->get_property ("shorten"), 0);

  SCM bemfl = ly_assoc_get (ly_symbol2scm ("beamed-extreme-minimum-free-lengths"),
                            details, SCM_EOL);

  Real minimum_free
    = (scm_is_pair (bemfl)
       ? (scm_to_double (robust_list_ref (beam_count - 1, bemfl))
          * staff_space
          * length_fraction)
       : 0.0);

  Real minimum_length = max (minimum_free, height_of_my_trem)
                        + height_of_my_beams
                        /* stem only extends to center of beam */
                        - 0.5 * beam_thickness;

  ideal_y *= my_dir;
  Real minimum_y = note_start + minimum_length;
  Real shortest_y = minimum_y * my_dir;

  return scm_list_2 (scm_from_double (ideal_y),
                     scm_from_double (shortest_y));
}

Slice
Stem::beam_multiplicity (Grob *stem)
{
  SCM beaming = stem->get_property ("beaming");
  Slice le = int_list_to_slice (scm_car (beaming));
  Slice ri = int_list_to_slice (scm_cdr (beaming));
  le.unite (ri);
  return le;
}

bool
Stem::is_cross_staff (Grob *stem)
{
  Grob *beam = unsmob_grob (stem->get_object ("beam"));
  return beam && Beam::is_cross_staff (beam);
}

MAKE_SCHEME_CALLBACK (Stem, calc_cross_staff, 1)
SCM
Stem::calc_cross_staff (SCM smob)
{
  return scm_from_bool (is_cross_staff (unsmob_grob (smob)));
}

Grob*
Stem::flag (Grob *me)
{
  return unsmob_grob (me->get_object ("flag"));
}

/* FIXME:  Too many properties  */
ADD_INTERFACE (Stem,
               "The stem represents the graphical stem.  In addition, it"
               " internally connects note heads, beams, and tremolos.  Rests"
               " and whole notes have invisible stems.\n"
               "\n"
               "The following properties may be set in the @code{details}"
               " list.\n"
               "\n"
               "@table @code\n"
               "@item beamed-lengths\n"
               "List of stem lengths given beam multiplicity.\n"
               "@item beamed-minimum-free-lengths\n"
               "List of normal minimum free stem lengths (chord to beams)"
               " given beam multiplicity.\n"
               "@item beamed-extreme-minimum-free-lengths\n"
               "List of extreme minimum free stem lengths (chord to beams)"
               " given beam multiplicity.\n"
               "@item lengths\n"
               "Default stem lengths.  The list gives a length for each"
               " flag count.\n"
               "@item stem-shorten\n"
               "How much a stem in a forced direction should be shortened."
               "  The list gives an amount depending on the number of flags"
               " and beams.\n"
               "@end table\n",

               /* properties */
               "avoid-note-head "
               "beam "
               "beaming "
               "beamlet-default-length "
               "beamlet-max-length-proportion "
               "default-direction "
               "details "
               "direction "
               "duration-log "
               "flag "
               "french-beaming "
               "length "
               "length-fraction "
               "max-beam-connect "
               "neutral-direction "
               "no-stem-extend "
               "note-heads "
               "positioning-done "
               "rests "
               "stem-begin-position "
               "stem-info "
               "stemlet-length "
               "thickness "
               "tremolo-flag "
              );

/****************************************************************/

Stem_info::Stem_info ()
{
  ideal_y_ = shortest_y_ = 0;
  dir_ = CENTER;
}

void
Stem_info::scale (Real x)
{
  ideal_y_ *= x;
  shortest_y_ *= x;
}
