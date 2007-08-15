/*
  dimensions-scheme.cc -- implement Dimension handling

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "lily-guile.hh"
#include "dimensions.hh"
#include "warn.hh"

LY_DEFINE (ly_pt, "ly:pt",
	   1, 0, 0, (SCM num),
	   "@var{num} printer points")
{
  SCM_ASSERT_TYPE (scm_is_number (num), num, SCM_ARG1, __FUNCTION__,
		   "number");
  return scm_from_double (point_constant
			  * scm_to_double (num));
}

LY_DEFINE (ly_cm, "ly:cm",
	   1, 0, 0, (SCM num),
	   "@var{num} cm")
{
  SCM_ASSERT_TYPE (scm_is_number (num), num, SCM_ARG1, __FUNCTION__,
		   "number");
  return scm_from_double (cm_constant
			  * scm_to_double (num));
}

LY_DEFINE (ly_inch, "ly:inch",
	   1, 0, 0, (SCM num),
	   "@var{num} inches")
{
  SCM_ASSERT_TYPE (scm_is_number (num), num, SCM_ARG1, __FUNCTION__,
		   "number");
  return scm_from_double (inch_constant
			  * scm_to_double (num));
}

LY_DEFINE (ly_mm, "ly:mm",
	   1, 0, 0, (SCM num),
	   "@var{num} mm")
{
  SCM_ASSERT_TYPE (scm_is_number (num), num, SCM_ARG1, __FUNCTION__,
		   "number");
  return scm_from_double (mm_constant
			  * scm_to_double (num));
}

LY_DEFINE (ly_bp, "ly:bp",
	   1, 0, 0, (SCM num),
	   "@var{num} bigpoints (1/72th inch)")
{
  SCM_ASSERT_TYPE (scm_is_number (num), num, SCM_ARG1, __FUNCTION__,
		   "number");
  return scm_from_double (bigpoint_constant
			  * scm_to_double (num));
}
