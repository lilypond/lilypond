/*
  stencil-scheme.cc -- implement Stencil

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "stencil.hh"
#include "font-metric.hh"

LY_DEFINE(ly_stencil_set_extent_x,"ly:stencil-set-extent!", 3 , 0, 0, 
	  (SCM mol, SCM axis, SCM np),
	  "Set the extent (@var{extent} must be a pair of numbers) of @var{mol} in \n"
"@var{axis} direction (0 or 1 for x- and y-axis respectively).\n"
"\n"
"Note that an extent @code{(A . B)} is an interval and hence @code{A} is\n"
"smaller than @code{B}, and is often negative.\n"
)
{
  Stencil* m = unsmob_stencil (mol);
  SCM_ASSERT_TYPE (m, mol, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE (is_number_pair (np), np, SCM_ARG3, __FUNCTION__, "number pair");

  Interval iv = ly_scm2interval (np);
  m->dim_[Axis (gh_scm2int (axis))] = iv;

  return SCM_UNDEFINED;
}


LY_DEFINE(ly_translate_stencil_axis,"ly:stencil-translate-axis", 3, 0, 0, 
	  (SCM mol, SCM amount, SCM axis),
	  "Return a @var{mol}, but translated by @var{amount} in @var{axis} direction")
{
  Stencil* m = unsmob_stencil (mol);
  SCM_ASSERT_TYPE (m, mol, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (gh_number_p (amount), amount, SCM_ARG2, __FUNCTION__, "number pair");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");


  Stencil q (*m);
  q.translate_axis (gh_scm2double (amount), Axis (gh_scm2int (axis)));

  return q.smobbed_copy();
}

LY_DEFINE(ly_translate_stencil,"ly:stencil-translate", 2, 0, 0, 
	  (SCM mol, SCM offset),
	  "Return a @var{mol}, but translated by @var{offset} (a pair of numbers).")
{
  Stencil* m = unsmob_stencil (mol);
  SCM_ASSERT_TYPE (m, mol, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (is_number_pair (offset), offset, SCM_ARG2, __FUNCTION__, "number pair");
  Offset o = ly_scm2offset (offset);
  
  Stencil q (*m);
  q.translate (o);
  return q.smobbed_copy();
}

LY_DEFINE(ly_stencil_get_extent,
	  "ly:stencil-get-extent", 2 , 0, 0,  (SCM mol, SCM axis),
	  "Return a pair of numbers signifying the extent of @var{mol} in "
"@var{axis} direction (0 or 1 for x and y axis respectively)."
)
{
  Stencil *m = unsmob_stencil (mol);
  SCM_ASSERT_TYPE (m, mol, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");
 
  return ly_interval2scm (m->extent (Axis (gh_scm2int (axis))));
}


LY_DEFINE(ly_stencil_combined_at_edge,
	  "ly:stencil-combine-at-edge",
	  4, 2, 0,  (SCM first, SCM axis, SCM direction,
		     SCM second,
		     SCM padding,
		     SCM minimum),
	  "Construct a stencil by putting @var{second} next to "
"@var{first}. @var{axis} can be 0 (x-axis) or 1 (y-axis), @var{direction} can be "
"-1 (left or down) or 1 (right or up). "
"The stencils are juxtaposed with  @var{padding} as extra space. If "
"this puts the reference points closer than @var{minimum}, they are moved "
"by the latter amount.")

{
  Stencil * m1 = unsmob_stencil (first);
  Stencil * m2 = unsmob_stencil (second);
  Stencil result;


  SCM_ASSERT_TYPE(is_axis (axis), axis, SCM_ARG3, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE(is_direction (direction), direction, SCM_ARG4, __FUNCTION__, "dir");

  Real p = 0.0;
  if (padding != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE(gh_number_p (padding), padding, SCM_ARG5, __FUNCTION__, "number");
      p = gh_scm2double (padding);
    }
  Real m =0.0;
  if (minimum != SCM_UNDEFINED)
    {
      SCM_ASSERT_TYPE(gh_number_p (minimum), minimum, SCM_ARG6, __FUNCTION__, "number");
      m = gh_scm2double (minimum);
    }
  
  if (m1)
    result = *m1;
  if (m2)
    result.add_at_edge (Axis (gh_scm2int (axis)), Direction (gh_scm2int (direction)),
			*m2, p, m);

  return result.smobbed_copy ();
}

/*
  FIXME: support variable number of arguments. 
  
 */
LY_DEFINE(ly_stencil_add , 
	  "ly:stencil-add", 0, 0, 1, (SCM args),
	  "Combine stencils. Takes any number of arguments."
	  )
{
#define FUNC_NAME __FUNCTION__
  SCM_VALIDATE_REST_ARGUMENT (args);

  Stencil result;

  while (!SCM_NULLP (args))
    {
      Stencil * m = unsmob_stencil (gh_car (args));

      if (!m)
	SCM_ASSERT_TYPE(m, gh_car (args), SCM_ARGn, __FUNCTION__,
			"Stencil");

      result.add_stencil (*m);

      args = gh_cdr (args);
    }
  
  return result.smobbed_copy ();
}

LY_DEFINE(ly_make_stencil,
	  "ly:make-stencil", 3, 0, 0,  (SCM expr, SCM xext, SCM yext),
	  " \n"
"The objective of any typesetting system is to put ink on paper in the \n"
"right places. For LilyPond, this final stage is left to the @TeX{} and \n"
"the printer subsystem. For lily, the last stage in processing a score is \n"
"outputting a description of what to put where.  This description roughly \n"
"looks like \n"
"@example \n"
"        PUT glyph AT (x,y) \n"
"        PUT glyph AT (x,y) \n"
"        PUT glyph AT (x,y)  \n"
"@end example \n"
"you merely have to look at the tex output of lily to see this. \n"
"Internally these instructions are encoded in Stencils.@footnote{At some \n"
"point LilyPond also contained Atom-objects, but they have been replaced \n"
"by Scheme expressions, making the name outdated.}  A stencil is \n"
"what-to-print-where information that also contains dimension information \n"
"(how large is this glyph?). \n"
" \n"
"Conceptually, Stencils can be constructed from Scheme code, by \n"
"translating a Stencil and by combining two stencils. In BNF \n"
"notation: \n"
" \n"
"@example \n"
"Stencil  :: COMBINE Stencil Stencil \n"
"           | TRANSLATE Offset Stencil \n"
"           | GLYPH-DESCRIPTION \n"
"           ; \n"
"@end example \n"
" \n"
"If you are interested in seeing how this information is stored, you \n"
"can run with the @code{-f scm} option. The scheme expressions are then \n"
"dumped in the output file.")
{
  SCM_ASSERT_TYPE (is_number_pair (xext), xext, SCM_ARG2, __FUNCTION__, "number pair");
  SCM_ASSERT_TYPE (is_number_pair (yext), yext, SCM_ARG3, __FUNCTION__, "number pair");  

  Box b (ly_scm2interval (xext), ly_scm2interval(yext));
  Stencil m (b, expr);
  return m.smobbed_copy ();
}


SCM
fontify_atom (Font_metric const * met, SCM f)
{
  if (f == SCM_EOL)
    return f;
  else
    return  scm_list_n (ly_symbol2scm ("fontify"),
			ly_quote_scm (met->description_), f, SCM_UNDEFINED);
}

LY_DEFINE(ly_fontify_atom,"ly:fontify-atom", 2, 0, 0, 
	  (SCM met, SCM f),
	  "Add a font selection command for the font metric @var{met} to @var{f}.")
{
  SCM_ASSERT_TYPE(unsmob_metrics (met), met, SCM_ARG1, __FUNCTION__, "font metric");

  return fontify_atom (unsmob_metrics (met), f);
}
LY_DEFINE(ly_align_to_x,"ly:stencil-align-to!", 3, 0, 0,  (SCM mol, SCM axis, SCM dir),

	  "Align @var{mol} using its own extents. @var{dir} is a number -1, 1 are "
	  " left and right respectively. Other values are interpolated (so 0 means "
	  " the center. ")
{
  SCM_ASSERT_TYPE(unsmob_stencil (mol), mol, SCM_ARG1, __FUNCTION__, "stencil");
  SCM_ASSERT_TYPE(is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");
  SCM_ASSERT_TYPE(gh_number_p (dir), dir, SCM_ARG3, __FUNCTION__, "number");

  unsmob_stencil (mol)->align_to ((Axis)gh_scm2int (axis),
				   gh_scm2double (dir));

  return SCM_UNDEFINED;
}
