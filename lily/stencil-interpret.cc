/*
  stencil-interpret.cc --  implement Stencil expression interpreting

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
