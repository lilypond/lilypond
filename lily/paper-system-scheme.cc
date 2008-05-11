/* 
  paper-system-scheme.cc -- implement Paper_system bindings.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2008 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "prob.hh"

#include "skyline-pair.hh"
  
LY_DEFINE (ly_paper_system_p, "ly:paper-system?",
	  1, 0, 0, (SCM obj),
	  "Type predicate.")
{
  return ly_prob_type_p (obj, ly_symbol2scm ("paper-system"));
}

LY_DEFINE (ly_paper_system_minimum_distance, "ly:paper-system-minimum-distance",
	   2, 0, 0, (SCM sys1, SCM sys2),
	   "Measure the minimum distance between these two paper-systems,"
	   " using their stored skylines if possible and falling back to"
	   " their extents otherwise.")
{
  Real ret = 0;
  Prob *p1 = unsmob_prob (sys1);
  Prob *p2 = unsmob_prob (sys2);
  Skyline_pair *sky1 = Skyline_pair::unsmob (p1->get_property ("vertical-skylines"));
  Skyline_pair *sky2 = Skyline_pair::unsmob (p2->get_property ("vertical-skylines"));

  if (sky1 && sky2)
    ret = (*sky1)[DOWN].distance ((*sky2)[UP]);
  else
    {
      Stencil *s1 = unsmob_stencil (p1->get_property ("stencil"));
      Stencil *s2 = unsmob_stencil (p2->get_property ("stencil"));
      Interval iv1 = s1->extent (Y_AXIS);
      Interval iv2 = s2->extent (Y_AXIS);
      ret = iv2[UP] - iv1[DOWN];
    }
  return scm_from_double (ret);
}
