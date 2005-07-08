/*
  font-interface-scheme.cc -- implement Font_interface bindings

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "font-interface.hh"

#include "grob.hh"


LY_DEFINE (ly_grob_default_font, "ly:grob-default-font",
	   1, 0, 0, (SCM grob),
	   "Return the default font for grob @var{gr}.")
{
  Grob *gr = unsmob_grob (grob);
  SCM_ASSERT_TYPE (gr, grob, SCM_ARG1, __FUNCTION__, "grob");

  return Font_interface::get_default_font (gr)->self_scm ();
}
