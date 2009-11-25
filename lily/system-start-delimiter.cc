/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  SCM fam = scm_cons (ly_symbol2scm ("font-encoding"),
		      ly_symbol2scm ("fetaMusic"));

  SCM alist = scm_list_n (fam, SCM_UNDEFINED);
  Font_metric *fm = select_font (me->layout (), scm_list_n (alist, SCM_UNDEFINED));

  Drul_array<Stencil> tips (fm->find_by_name ("brackettips.down"),
			    fm->find_by_name ("brackettips.up"));

  Real thickness = robust_scm2double (me->get_property ("thickness"), 0.25);

  Real overlap = 0.1 * thickness;

  Box box (Interval (0, thickness),
	   Interval (-1, 1)
	   * (height / 2 + overlap));
  
  Stencil bracket = Lookup::filled_box (box);
  Direction d = DOWN;
  do
    bracket.add_at_edge (Y_AXIS, d, tips[d], -overlap);
  while (flip (&d) != DOWN);
  bracket = Stencil (box, bracket.expr ());

  bracket.translate_axis (-0.8, X_AXIS);
  
  return bracket;
}

Stencil
System_start_delimiter::line_bracket (Grob *me, Real height)
{
  Real thick
    = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"))
    * robust_scm2double (me->get_property ("thickness"), 1);
  Real w = 0.8;
  
  Stencil tip1 = Line_interface::make_line (thick,
					   Offset (0, -height/2),
					   Offset (w, -height/2));
  Stencil tip2 = Line_interface::make_line (thick,
					    Offset (0, height/2),
					    Offset (w, height/2));
  Stencil vline = Line_interface::make_line (thick,
					     Offset (0, -height/2),
					     Offset (0, height/2));

  vline.add_stencil (tip1);
  vline.add_stencil (tip2);
  vline.translate_axis (-w, X_AXIS);
  return vline;
}

Stencil
System_start_delimiter::simple_bar (Grob *me, Real h)
{
  Real lt = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real w = lt * robust_scm2double (me->get_property ("thickness"), 1);
  return Lookup::round_filled_box (Box (Interval (0, w), Interval (-h / 2, h / 2)),
				   lt);
}

MAKE_SCHEME_CALLBACK (System_start_delimiter, print, 1);
SCM
System_start_delimiter::print (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);
  extract_grob_set (me, "elements", elts);
  Grob *common = common_refpoint_of_array (elts, me, Y_AXIS);

  Interval ext;
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
	      non_empty_count ++;
	      ext.unite (dims);
	    }
	}
    }

  SCM glyph_sym = me->get_property ("style");
  Real len = ext.length ();
  if (ext.is_empty ()
      || (robust_scm2double (me->get_property ("collapse-height"), 0.0) >= ext.length ()))
    {
      me->suicide ();
      return SCM_UNSPECIFIED;
    }

  Stencil m;
  if (glyph_sym == ly_symbol2scm ("bracket"))
    m = staff_bracket (me, len);
  else if (glyph_sym == ly_symbol2scm ("brace"))
    m = staff_brace (me, len);
  else if (glyph_sym == ly_symbol2scm ("bar-line"))
    m = simple_bar (me, len);
  else if (glyph_sym == ly_symbol2scm ("line-bracket"))
    m = line_bracket (me, len);

  m.translate_axis (ext.center (), Y_AXIS);
  return m.smobbed_copy ();
}

Stencil
System_start_delimiter::staff_brace (Grob *me, Real y)
{
  Font_metric *fm = 0;

  /*
    Find the default brace font if the user overrides it.
  */
  fm = Font_interface::get_default_font (me);

  int
    lo = 0;
  int hi = max ((int) fm->count () - 1, 2);

  /* do a binary search for each Y, not very efficient, but passable?  */
  Box b;
  do
    {
      int cmp = (lo + hi) / 2;
      b = fm->get_indexed_char (cmp);
      if (b[Y_AXIS].is_empty () || b[Y_AXIS].length () > y)
	hi = cmp;
      else
	lo = cmp;
    }
  while (hi - lo > 1);

  Stencil stil (fm->find_by_name ("brace" + to_string (lo)));
  stil.translate_axis (-b[X_AXIS].length ()/2, X_AXIS);

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
