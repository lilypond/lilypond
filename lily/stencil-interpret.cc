/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "stencil.hh"

void
interpret_stencil_expression (SCM expr,
			      void (*func) (void *, SCM),
			      void *func_arg,
			      Offset o)
{
  while (1)
    {
      if (!scm_is_pair (expr))
	return;

      SCM head = scm_car (expr);

      if (head == ly_symbol2scm ("delay-stencil-evaluation"))
	{
	  interpret_stencil_expression (scm_force (scm_cadr (expr)), func, func_arg, o);
	  return;
	}
      if (head == ly_symbol2scm ("translate-stencil"))
	{
	  o += ly_scm2offset (scm_cadr (expr));
	  expr = scm_caddr (expr);
	}
      else if (head == ly_symbol2scm ("combine-stencil"))
	{

	  for (SCM x = scm_cdr (expr); scm_is_pair (x); x = scm_cdr (x))
	    interpret_stencil_expression (scm_car (x), func, func_arg, o);
	  return;
	}
      else if (head == ly_symbol2scm ("grob-cause"))
	{
	  SCM grob = scm_cadr (expr);

	  (*func) (func_arg, scm_list_3 (head,
					 ly_quote_scm (ly_offset2scm (o)), grob));
	  interpret_stencil_expression (scm_caddr (expr), func, func_arg, o);
	  (*func) (func_arg, scm_list_1 (ly_symbol2scm ("no-origin")));
	  return;
	}
      else if (head == ly_symbol2scm ("color"))
	{
	  SCM color = scm_cadr (expr);
	  SCM r = scm_car (color);
	  SCM g = scm_cadr (color);
	  SCM b = scm_caddr (color);

	  (*func) (func_arg, scm_list_4 (ly_symbol2scm ("setcolor"), r, g, b));
	  interpret_stencil_expression (scm_caddr (expr), func, func_arg, o);
	  (*func) (func_arg, scm_list_1 (ly_symbol2scm ("resetcolor")));

	  return;
	}
      else if (head == ly_symbol2scm ("rotate-stencil"))
	{
	  SCM args = scm_cadr (expr);
	  SCM angle = scm_car (args);
	  Offset tmp = o + robust_scm2offset (scm_cadr (args), Offset (0.0, 0.0));

	  SCM offset = ly_offset2scm (tmp);
	  SCM x = scm_car (offset);
	  SCM y = scm_cdr (offset);

	  (*func) (func_arg, scm_list_4 (ly_symbol2scm ("setrotation"), angle, x, y));
	  interpret_stencil_expression (scm_caddr (expr), func, func_arg, o);
	  (*func) (func_arg, scm_list_4 (ly_symbol2scm ("resetrotation"), angle, x, y));

	  return;
	}
      else
	{
	  (*func) (func_arg,
		   scm_list_4 (ly_symbol2scm ("placebox"),
			       scm_from_double (o[X_AXIS]),
			       scm_from_double (o[Y_AXIS]),
			       expr));
	  return;
	}
    }
}

struct Font_list
{
  SCM fonts_;
};

static void
find_font_function (void *fs, SCM x)
{
  Font_list *me = (Font_list *) fs;

  if (scm_car (x) == ly_symbol2scm ("placebox"))
    {
      SCM args = scm_cdr (x);
      SCM what = scm_caddr (args);

      if (scm_is_pair (what))
	{
	  SCM head = scm_car (what);
	  if (ly_symbol2scm ("text") == head)
	    me->fonts_ = scm_cons (scm_cadr (what), me->fonts_);
	  else if (head == ly_symbol2scm ("char"))
	    me->fonts_ = scm_cons (scm_cadr (what), me->fonts_);
	}
    }
}

SCM
find_expression_fonts (SCM expr)
{
  Font_list fl;

  fl.fonts_ = SCM_EOL;

  interpret_stencil_expression (expr, &find_font_function,
				(void *) & fl, Offset (0, 0));

  return fl.fonts_;
}
