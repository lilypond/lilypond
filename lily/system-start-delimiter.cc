/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "system-start-delimiter.hh"
#include "text-interface.hh"
#include "all-font-metrics.hh"
#include "axis-group-interface.hh"
#include "font-interface.hh"
#include "item.hh"
#include "line-interface.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"

Stencil
System_start_delimiter::staff_bracket (Grob *me, Real height)
{
  Font_metric *fm = Font_interface::get_default_font (me);

  Drul_array<Stencil> tips (fm->find_by_name ("brackettips.down"),
                            fm->find_by_name ("brackettips.up"));

  Real thickness = from_scm<double> (get_property (me, "thickness"), 0.25);

  Real overlap = 0.1 * thickness;

  Box bracket_line_extents (Interval (0, thickness),
                            Interval (-1, 1) * (height / 2 + overlap));

  Stencil bracket = Lookup::filled_box (bracket_line_extents);
  for (DOWN_and_UP (d))
    bracket.add_at_edge (Y_AXIS, d, tips[d], -overlap);

  // The reference for positioning the delimiter in X-direction should
  // be the bracket line, not the right bound of the bracket tips.
  // In Y-direction we have to take the tips into account, however,
  // to ensure correct bounding boxes with the EPS backend.
  // Therefore we take the X-dimensions only from the bracket line
  // and the Y-dimensions from the whole bracket.
  Box bracket_extents (bracket_line_extents[X_AXIS], bracket.extent (Y_AXIS));
  bracket = Stencil (bracket_extents, bracket.expr ());

  bracket.translate_axis (-0.8, X_AXIS);

  return bracket;
}

Stencil
System_start_delimiter::line_bracket (Grob *me, Real height)
{
  Real thick
    = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"))
      * from_scm<double> (get_property (me, "thickness"), 1);
  Real w = 0.8;

  Stencil tip1 = Line_interface::make_line (thick,
                                            Offset (0, -height / 2),
                                            Offset (w, -height / 2));
  Stencil tip2 = Line_interface::make_line (thick,
                                            Offset (0, height / 2),
                                            Offset (w, height / 2));
  Stencil vline = Line_interface::make_line (thick,
                                             Offset (0, -height / 2),
                                             Offset (0, height / 2));

  vline.add_stencil (tip1);
  vline.add_stencil (tip2);
  vline.translate_axis (-w, X_AXIS);
  return vline;
}

Stencil
System_start_delimiter::simple_bar (Grob *me, Real h)
{
  Real lt = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real w = lt * from_scm<double> (get_property (me, "thickness"), 1);
  return Lookup::round_filled_box (Box (Interval (0, w), Interval (-h / 2, h / 2)),
                                   lt);
}

MAKE_SCHEME_CALLBACK (System_start_delimiter, print, 1);
SCM
System_start_delimiter::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  extract_grob_set (me, "elements", elts);
  Grob *common = common_refpoint_of_array (elts, me, Y_AXIS);

  Interval ext;
  Real staffspace = 1.0;
  int non_empty_count = 0;
  for (vsize i = elts.size (); i--;)
    {
      Spanner *sp = dynamic_cast<Spanner *> (elts[i]);

      if (sp
          && sp->get_bound (LEFT) == me->get_bound (LEFT))
        {
          Interval dims = sp->extent (common, Y_AXIS);
          if (!dims.is_empty ())
            {
              non_empty_count++;
              ext.unite (dims);
              staffspace = Staff_symbol_referencer::staff_space (sp);
            }
        }
    }

  SCM glyph_sym = get_property (me, "style");
  Real len = ext.length ();

  // Use collapse-height in multiples of the staff-space
  if (ext.is_empty ()
      || (from_scm<double> (get_property (me, "collapse-height"), 0.0) >= (len / staffspace)))
    {
      me->suicide ();
      return SCM_UNSPECIFIED;
    }

  Stencil m;
  if (scm_is_eq (glyph_sym, ly_symbol2scm ("bracket")))
    m = staff_bracket (me, len);
  else if (scm_is_eq (glyph_sym, ly_symbol2scm ("brace")))
    m = staff_brace (me, len);
  else if (scm_is_eq (glyph_sym, ly_symbol2scm ("bar-line")))
    m = simple_bar (me, len);
  else if (scm_is_eq (glyph_sym, ly_symbol2scm ("line-bracket")))
    m = line_bracket (me, len);

  m.translate_axis (ext.center (), Y_AXIS);
  return m.smobbed_copy ();
}

Stencil
System_start_delimiter::staff_brace (Grob *me, Real y)
{
  Font_metric *fm = 0;
  /* We use the style sheet to look up the font file name.
     This is better than using 'find_font' directly.*/
  SCM fam = scm_cons (ly_symbol2scm ("font-encoding"),
                      ly_symbol2scm ("fetaBraces"));

  SCM alist = scm_list_n (fam, SCM_UNDEFINED);
  fm = select_font (me->layout (), scm_list_n (alist, SCM_UNDEFINED));

  int
  lo = 0;
  int hi = std::max ((int) fm->count () - 1, 2);

  /* do a binary search for each Y, not very efficient, but passable?  */
  Box b;
  do
    {
      int cmp = (lo + hi) / 2;
      b = fm->get_indexed_char_dimensions (cmp);
      if (b[Y_AXIS].is_empty () || b[Y_AXIS].length () > y)
        hi = cmp;
      else
        lo = cmp;
    }
  while (hi - lo > 1);

  Stencil stil (fm->find_by_name ("brace" + std::to_string (lo)));
  stil.translate_axis (-b[X_AXIS].length () / 2, X_AXIS);

  stil.translate_axis (-0.2, X_AXIS);

  return stil;
}

ADD_INTERFACE (System_start_delimiter,
               "The brace, bracket or bar in front of the system.  The"
               " following values for @code{style} are recognized:\n"
               "\n"
               "@table @code\n"
               "@item bracket\n"
               "A thick bracket, normally used to group similar"
               " instruments in a score.  Default for @code{StaffGroup}."
               "  @code{SystemStartBracket} uses this style.\n"
               "@item brace\n"
               "A @q{piano style} brace normally used for an instrument"
               " that uses two staves.  The default style for"
               " @code{GrandStaff}.  @code{SystemStartBrace} uses this"
               " style.\n"
               "@item bar-line\n"
               "A simple line between the staves in a score.  Default"
               " for staves enclosed in @code{<<} and @code{>>}."
               "  @code{SystemStartBar} uses this style.\n"
               "@item line-bracket\n"
               "A simple square, normally used for subgrouping"
               " instruments in a score.  @code{SystemStartSquare} uses"
               " this style.\n"
               "@end table\n"
               "\n"
               "See also @file{input/regression/system-start-nesting.ly}.",

               /* properties */
               "collapse-height "
               "style "
               "thickness "
              );
