/*
  rhythmic-head.cc -- implement Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "rhythmic-head.hh"
#include "debug.hh"
#include "rest.hh"
#include "stem.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"


Item*
Rhythmic_head::dots_l (Grob*me) 
{
  SCM s = me->get_grob_property ("dot");
  return unsmob_item (s);
}

Item*
Rhythmic_head::stem_l (Grob*me) 
{
  SCM s = me->get_grob_property ("stem");
  return unsmob_item (s);
}

int
Rhythmic_head::dot_count (Grob*me) 
{
  return dots_l (me)
    ? gh_scm2int (dots_l (me)->get_grob_property ("dot-count")) : 0;
}

void
Rhythmic_head::set_dots (Grob*me,Item *dot_l)
{
  me->set_grob_property ("dot", dot_l->self_scm ());
}



int
Rhythmic_head::duration_log (Grob*me) 
{
  SCM s = me->get_grob_property ("duration-log");
  return gh_number_p (s) ? gh_scm2int (s) : 0;
}



ADD_INTERFACE (Rhythmic_head,"rhythmic-head-interface",
  "Note head or rest",
  "dot stem duration-log");

