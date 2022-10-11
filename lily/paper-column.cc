/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "paper-column.hh"

#include "axis-group-interface.hh"
#include "break-align-interface.hh"
#include "engraver.hh"
#include "font-interface.hh"
#include "grob-array.hh"
#include "international.hh"
#include "lookup.hh"
#include "ly-scm-list.hh"
#include "moment.hh"
#include "output-def.hh"
#include "paper-score.hh"
#include "pointer-group-interface.hh"
#include "rhythmic-head.hh"
#include "separation-item.hh"
#include "skyline-pair.hh"
#include "spaceable-grob.hh"
#include "spring.hh"
#include "string-convert.hh"
#include "system.hh"
#include "text-interface.hh"
#include "warn.hh"

using std::string;
using std::vector;

bool
Paper_column::internal_set_as_bound_of_spanner (Spanner *s, Direction)
{
  bool ok = s->accepts_as_bound_paper_column (this);
  if (ok)
    {
      // Signal that this column needs to be kept alive. They need to be kept
      // alive to have meaningful position and linebreaking.  [maybe we should
      // try keeping all columns alive?, and perhaps inherit position from
      // their (non-)musical brother]
      Pointer_group_interface::add_grob (this, ly_symbol2scm ("bounded-by-me"),
                                         s);
    }
  return ok;
}

void
Paper_column::set_rank (int rank)
{
  rank_ = rank;
}

System *
Paper_column::get_system () const
{
  return system_;
}

void
Paper_column::set_system (System *s)
{
  system_ = s;
}

Paper_column *
Paper_column::get_column () const
{
  return const_cast<Paper_column *> (this);
}

Paper_column::Paper_column (SCM l)
  : Item (l)
{
  system_ = 0;
  rank_ = -1;
  add_interface (ly_symbol2scm ("paper-column-interface"));
}

Paper_column::Paper_column (Paper_column const &src)
  : Item (src)
{
  system_ = 0;
  rank_ = src.rank_;
}

Moment
Paper_column::when_mom (Grob *me)
{
  SCM m = get_property (me, "when");
  if (Moment *when = unsmob<Moment> (m))
    return *when;
  return Moment (0);
}

bool
Paper_column::is_musical (Grob *me)
{
  SCM m = get_property (me, "shortest-starter-duration");
  if (Moment *s = unsmob<Moment> (m))
    return *s != Moment (0);
  return false;
}

bool
Paper_column::is_used (Grob *me)
{
  extract_grob_set (me, "elements", elts);
  if (elts.size ())
    return true;

  extract_grob_set (me, "bounded-by-me", bbm);
  if (bbm.size ())
    return true;

  if (Paper_column::is_breakable (me))
    return true;

  if (from_scm<bool> (get_property (me, "used")))
    return true;

  if (scm_is_pair (get_property (me, "labels")))
    return true;

  return false;
}

bool
Paper_column::is_breakable (Grob *me)
{
  return scm_is_symbol (get_property (me, "line-break-permission"));
}

Real
Paper_column::minimum_distance (Grob *left, Grob *right)
{
  Drul_array<Grob *> cols (left, right);
  Drul_array<Skyline> skys
    = Drul_array<Skyline> (Skyline (RIGHT), Skyline (LEFT));

  for (const auto d : {LEFT, RIGHT})
    {
      SCM sp_scm = get_property (cols[d], "horizontal-skylines");
      if (is_scm<Skyline_pair> (sp_scm))
        {
          const Skyline_pair &sp = from_scm<Skyline_pair> (sp_scm);
          skys[d] = sp[-d];
        }
    }

  skys[RIGHT].merge (Separation_item::conditional_skyline (right, left));

  return std::max (0.0, skys[LEFT].distance (skys[RIGHT]));
}

Interval
Paper_column::break_align_width (Grob *me, SCM align_syms)
{
  Grob *sys = me->get_x_parent ();
  Real coord = me->relative_coordinate (sys, X_AXIS);

  if (scm_is_symbol (align_syms))
    align_syms = ly_list (align_syms);

  if (is_musical (me))
    {
      me->programming_error (
        "tried to get break-align-width of a musical column");
      return Interval (coord);
    }

  auto *break_alignment = unsmob<Item> (get_object (me, "break-alignment"));
  if (!break_alignment)
    return Interval (coord);

  Grob *align = nullptr;
  for (SCM align_sym : as_ly_scm_list (align_syms))
    {
      if (scm_is_eq (align_sym, ly_symbol2scm ("break-alignment")))
        {
          align = break_alignment;
          // No need for an X-extent check: if the whole BreakAlignment has
          // empty extent, none of its BreakAlignGroups will have non-empty
          // extent.
          break;
        }
      else
        {
          Grob *group
            = Break_alignment_interface::find_nonempty_break_align_group (
              break_alignment, align_sym);
          if (group)
            {
              align = group;
              break;
            }
        }
    }

  if (!align)
    return Interval (coord);

  Interval ext = align->extent (me, X_AXIS);
  if (ext.is_empty ())
    return Interval (coord);

  return ext + coord;
}

LY_DEFINE (ly_paper_column__break_align_width,
           "ly:paper-column::break-align-width", 2, 0, 0,
           (SCM col, SCM align_syms),
           R"(

@var{col} should be a non-musical paper-column.  This function
determines the horizontal extent of a break align group contained in
this column, relative to the system.  The break align group is
searched according to @var{align-sym}, which is either a break align
symbol (see the @code{break-align-symbol} property), or a list of such
symbols.  For example,

@example
(ly:paper-column::break-align-width col '(key-signature staff-bar))
@end example

@noindent
tries to find a @code{BreakAlignGroup} of key signatures, but falls
back on bar lines if there are no key signatures or if the extent of
the @code{BreakAlignGroup} containing them is empty (for example, if
they are omitted).

The special symbol @code{break-alignment} means the combined extent of
all items in the paper column.  It is useful as the last element of
the list, for a catch-all fallback.

This function never returns an empty interval.  If no matching group
is found or the group has an empty extent, it returns a point interval
at the coordinate of the column relative to the system.
           )")
{
  auto *const me = LY_ASSERT_SMOB (Grob, col, 1);
  SCM_ASSERT_TYPE (scm_is_symbol (align_syms) || ly_is_list (align_syms),
                   align_syms, SCM_ARG2, __FUNCTION__, "symbol or list");

  Interval ext = Paper_column::break_align_width (me, align_syms);
  return to_scm (ext);
}

/*
  Loop through elements of a PaperColumn, find all grobs implementing specified
  interface and return their combined extent.
*/
Interval
Paper_column::get_interface_extent (Grob *column, SCM iface, Axis a)
{
  Interval extent = Interval (0, 0);
  extract_grob_set (column, "elements", elts);

  for (vsize i = 0; i < elts.size (); i++)
    if (elts[i]->internal_has_interface (iface))
      extent.unite (robust_relative_extent (elts[i], elts[i], a));

  return extent;
}

/*
  Print a:
  - vertical line,
  - the rank number,
  - rank moment,
  - blue arrow representing ideal distance,
  - red arrow representing minimum distance
  to aid debugging.  To turn this on, simply add
  \override Score.PaperColumn.stencil = #ly:paper-column::print
  \override Score.NonMusicalPaperColumn.stencil = #ly:paper-column::print
  to your score.
  Also, as of 2013-10-16 there's a switch in Frescobaldi that turns this on.
*/
MAKE_DOCUMENTED_SCHEME_CALLBACK (Paper_column, print, "ly:paper-column::print",
                                 1,
                                 "Optional stencil for @code{PaperColumn} or"
                                 " @code{NonMusicalPaperColumn}."
                                 "  Draws the @dfn{rank number} of each column,"
                                 " its moment in time, a blue arrow showing the"
                                 " ideal distance, and a red arrow showing the"
                                 " minimum distance between columns.");
SCM
Paper_column::print (SCM p)
{
  Paper_column *me = unsmob<Paper_column> (p);

  string r = std::to_string (me->get_rank ());

  Moment *mom = unsmob<Moment> (get_property (me, "when"));
  string when = mom ? mom->to_string () : "?/?";

  Font_metric *musfont = Font_interface::get_default_font (me);
  SCM properties = Font_interface::text_font_alist_chain (me);
  auto t = Text_interface::interpret_markup (me->layout (), properties,
                                             ly_string2scm (r));
  auto when_mol = Text_interface::interpret_markup (me->layout (), properties,
                                                    ly_string2scm (when));
  t.scale (1.2, 1.4);
  t.add_at_edge (Y_AXIS, DOWN, when_mol, 0.1);
  t.align_to (X_AXIS, LEFT);
  // compensate for font serifs and half letter-distance
  t.translate (Offset (-0.1, 0));
  t.align_to (Y_AXIS, DOWN);

  Stencil l = Lookup::filled_box (Box (Interval (0, 0.02), Interval (-8, -1)));

  Real small_pad = 0.15;
  Real big_pad = 0.35;

  // number of printed arrows from *both* loops
  int j = 0;

  for (SCM s = get_object (me, "ideal-distances"); scm_is_pair (s);
       s = scm_cdr (s))
    {
      Spring *sp = unsmob<Spring> (scm_caar (s));
      if (!unsmob<Grob> (scm_cdar (s))
          || !unsmob<Grob> (scm_cdar (s))->get_system ())
        continue;

      j++;

      Stencil arrowhead (musfont->find_by_name ("arrowheads.open.01"));
      // initial scaling; it will also scale with font-size.
      arrowhead.scale (1, 1.66);
      Real head_len = arrowhead.extent (X_AXIS).length ();

      SCM number_markup = ly_string2scm (
        String_convert::form_string ("%5.2lf", sp->ideal_distance ()));
      auto number_stc = Text_interface::interpret_markup (
        me->layout (), properties, number_markup);
      number_stc.scale (1, 1.1);
      Real num_height = number_stc.extent (Y_AXIS).length ();
      Real num_len = number_stc.extent (X_AXIS).length ();
      number_stc.align_to (Y_AXIS, DOWN);

      // arrow's y-coord relative to the top of l stencil:
      Real y = -2.5;
      y -= j * (num_height + small_pad + big_pad);
      // horizontally center number on the arrow, excluding arrowhead.
      Offset num_off = Offset ((sp->ideal_distance () - num_len - head_len) / 2,
                               y + small_pad);

      vector<Offset> pts;
      pts.push_back (Offset (0, y));

      Offset p2 (sp->ideal_distance (), y);
      pts.push_back (p2);

      Stencil id_stencil = Lookup::points_to_line_stencil (0.1, pts);
      id_stencil.add_stencil (arrowhead.translated (p2));
      id_stencil.add_stencil (number_stc.translated (num_off));
      // use a lighter shade of blue so it will remain legible on black background.
      id_stencil = id_stencil.in_color (0.2, 0.4, 1.0);
      l.add_stencil (id_stencil);
    }

  for (SCM s = get_object (me, "minimum-distances"); scm_is_pair (s);
       s = scm_cdr (s))
    {
      Real dist = from_scm<double> (scm_cdar (s));
      Grob *other = unsmob<Grob> (scm_caar (s));
      if (!other || other->get_system () != me->get_system ())
        continue;

      j++;

      Stencil arrowhead (musfont->find_by_name ("arrowheads.open.01"));
      // initial scaling; it will also scale with font-size.
      arrowhead.scale (1, 1.66);
      Real head_len = arrowhead.extent (X_AXIS).length ();

      SCM number_markup
        = ly_string2scm (String_convert::form_string ("%5.2lf", dist));
      auto number_stc = Text_interface::interpret_markup (
        me->layout (), properties, number_markup);
      number_stc.scale (1, 1.1);
      Real num_height = number_stc.extent (Y_AXIS).length ();
      Real num_len = number_stc.extent (X_AXIS).length ();
      number_stc.align_to (Y_AXIS, UP);

      // arrow's y-coord relative to the top of l stencil:
      Real y = -3;
      y -= j * (num_height + small_pad + big_pad);
      // horizontally center number on the arrow, excluding arrowhead.
      Offset num_off = Offset ((dist - num_len - head_len) / 2, y - small_pad);

      vector<Offset> pts;
      pts.push_back (Offset (0, y));

      Offset p2 (dist, y);
      pts.push_back (p2);

      Stencil id_stencil = Lookup::points_to_line_stencil (0.1, pts);
      id_stencil.add_stencil (arrowhead.translated (p2));
      id_stencil.add_stencil (number_stc.translated (num_off));
      // use a lighter shade of red so it will remain legible on black background.
      id_stencil = id_stencil.in_color (1.0, 0.25, 0.25);
      l.add_stencil (id_stencil);
    }
  t.add_stencil (l);
  return t.smobbed_copy ();
}

/* FIXME: This is a hack that we use to identify columns that used to
   contain note-heads but whose note-heads were moved by one of the ligature
   engravers. Once the ligature engravers are fixed to behave nicely, this
   function can be removed.
*/
bool
Paper_column::is_extraneous_column_from_ligature (Grob *me)
{
  if (!is_musical (me))
    return false;

  // If all the note-heads that I think are my children actually belong
  // to another column, then I am extraneous.
  extract_grob_set (me, "elements", elts);
  bool has_notehead = false;
  for (vsize i = 0; i < elts.size (); i++)
    {
      if (has_interface<Rhythmic_head> (elts[i]))
        {
          has_notehead = true;
          if (dynamic_cast<Item *> (elts[i])->get_column () == me)
            return false;
        }
    }
  return has_notehead;
}

[[noreturn]] Paper_column *
Paper_column::make_sticky_same_type (Engraver * /*eng*/, SCM /*type*/,
                                     SCM /*cause*/, char const * /*file*/,
                                     int /*line*/, char const * /*fun*/)
{
  error (_f ("refusing to create a grob sticking to Paper_column"
             " grob %s; paper columns should not be created from"
             " custom engravers.",
             this->name ()));
}

ADD_INTERFACE (Paper_column,
               R"(
@code{Paper_column} objects form the top-most X@tie{}parents for items.  There
are two types of columns: musical and non-musical, to which musical and
non-musical objects are attached respectively.  The spacing engine determines
the X@tie{}positions of these objects.

They are numbered, the first (leftmost) is column@tie{}0.  Numbering happens
before line breaking, and columns are not renumbered after line breaking.
Since many columns go unused, you should only use the rank field to get
ordering information.  Two adjacent columns may have non-adjacent numbers.

The @code{paper-column-interface} implies the @iref{item-interface}.
               )",

               /* properties */
               R"(
bounded-by-me
labels
maybe-loose
rhythmic-location
spacing
used
when
               )");
