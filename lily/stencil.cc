/*
  stencil.cc -- implement Stencil

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "stencil.hh"

#include <math.h>

#include <libc-extension.hh>	// isinf

#include "input-smob.hh"
#include "font-metric.hh"
#include "dimensions.hh"
#include "string-convert.hh"
#include "warn.hh"

#include "ly-smobs.icc"

Stencil::Stencil ()
{
  expr_ = SCM_EOL;
  set_empty (true);
}

Stencil::Stencil (Box b, SCM func)
{
  expr_ = func;
  dim_ = b;
}

int
Stencil::print_smob (SCM, SCM port, scm_print_state *)
{
  scm_puts ("#<Stencil ", port);
  scm_puts (" >", port);
  return 1;
}

SCM
Stencil::mark_smob (SCM smob)
{
  Stencil *s = (Stencil*) SCM_CELL_WORD_1 (smob);
  return s->expr_;
}

IMPLEMENT_SIMPLE_SMOBS (Stencil);
IMPLEMENT_TYPE_P (Stencil, "ly:stencil?");
IMPLEMENT_DEFAULT_EQUAL_P (Stencil);

Interval
Stencil::extent (Axis a) const
{
  return dim_[a];
}

/* Hmm... maybe this is not such a good idea ; stuff can be empty,
   while expr_ == '()  */
bool
Stencil::is_empty () const
{
  return expr_ == SCM_EOL;
}

SCM
Stencil::expr () const
{
  return expr_;
}

Box
Stencil::extent_box () const
{
  return dim_;
}
Offset
Stencil::origin () const
{
  return origin_;
}

void
Stencil::translate (Offset o)
{
  Axis a = X_AXIS;
  while (a < NO_AXES)
    {
      if (isinf (o[a]) || isnan (o[a]))
	{
	  programming_error (String_convert::form_string ("Improbable offset for stencil: %f staff space", o[a])
			     + "\n"
			     + "Setting to zero.");
	  o[a] =  0.0;
	}
      incr (a);
    }

  expr_ = scm_list_n (ly_symbol2scm ("translate-stencil"),
		      ly_offset2scm (o),
		      expr_, SCM_UNDEFINED);
  if (!is_empty ())
    dim_.translate (o);
  origin_ += o;
}

void
Stencil::translate_axis (Real x, Axis a)
{
  Offset o (0, 0);
  o[a] = x;
  translate (o);
}

void
Stencil::add_stencil (Stencil const &s)
{
  expr_ = scm_list_3 (ly_symbol2scm ("combine-stencil"), s.expr_, expr_);
  dim_.unite (s.dim_);
}



void
Stencil::set_empty (bool e)
{
  if (e)
    {
      dim_[X_AXIS].set_empty ();
      dim_[Y_AXIS].set_empty ();
    }
  else
    {
      dim_[X_AXIS] = Interval (0, 0);
      dim_[Y_AXIS] = Interval (0, 0);
    }
}

void
Stencil::align_to (Axis a, Real x)
{
  if (is_empty ())
    return;

  Interval i (extent (a));
  translate_axis (-i.linear_combination (x), a);
}

/* FIXME: unintuitive naming, you would expect *this to be moved.
   Kept (keeping?) API for compat with add_at_edge ().

   What is PADDING, what is MINIMUM, exactly?  */
Stencil
Stencil::moved_to_edge (Axis a, Direction d, Stencil const &s,
			Real padding, Real minimum) const
{
  Interval my_extent = dim_[a];
  Interval i (s.extent (a));
  Real his_extent;
  if (i.is_empty ())
    {
      programming_error ("Stencil::moved_to_edge: adding empty stencil.");
      his_extent = 0.0;
      //      SCM_ASSERT_TYPE (0, s.expr (), SCM_ARG1, __FUNCTION__, "non-empty stencil");
    }
  else
    his_extent = i[-d];

  Real offset = (my_extent.is_empty () ? 0.0 : my_extent[d] - his_extent)
    + d * padding;

  Stencil toadd (s);
  toadd.translate_axis (offset, a);

  if (minimum > 0 && d * (-origin ()[a] + toadd.origin ()[a]) < minimum)
    toadd.translate_axis ( -toadd.origin ()[a]
			   + origin ()[a] + d * minimum, a);

  return toadd;
}

/*  See scheme Function.  */
void
Stencil::add_at_edge (Axis a, Direction d, Stencil const &s, Real padding,
		      Real minimum)
{
  add_stencil (moved_to_edge (a, d, s, padding, minimum));
}


/****************************************************************/


void
interpret_stencil_expression (SCM expr,
			      void (*func) (void*, SCM),
			      void *func_arg,
			      Offset o)
{
  while (1)
    {
      if (!scm_is_pair (expr))
        return;

      SCM head = scm_car (expr);

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
      else
        {
          (*func) (func_arg,
                   scm_list_4 (ly_symbol2scm ("placebox"),
                               scm_make_real (o[X_AXIS]),
                               scm_make_real (o[Y_AXIS]),
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
  Font_list *me = (Font_list*) fs;

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
				(void*) &fl, Offset (0, 0));

  return fl.fonts_;
}


