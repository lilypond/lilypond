/*
  rhythmic-head.cc -- implement Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "rhythmic-head.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "rest.hh"
#include "dots.hh"
#include "paper-score.hh"
#include "stem.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"


Item*
Rhythmic_head::dots_l (Score_element*me) 
{
  SCM s = me->get_elt_property ("dot");
  return dynamic_cast<Item*> (unsmob_element (s));
}

int
Rhythmic_head::balltype_i (Score_element*me) 
{
  SCM s = me->get_elt_property ("duration-log");
  
  return gh_number_p (s) ? gh_scm2int (s) : 0;
}

Item*
Rhythmic_head::stem_l (Score_element*me) 
{
  SCM s = me->get_elt_property ("stem");
  return dynamic_cast<Item*> (unsmob_element (s));
}

int
Rhythmic_head::dot_count (Score_element*me) 
{
  return dots_l (me)
    ? gh_scm2int (dots_l (me)->get_elt_property ("dot-count")) : 0;
}

void
Rhythmic_head::set_dots (Score_element*me,Item *dot_l)
{
  me->set_elt_property ("dot", dot_l->self_scm_);
}


void
Rhythmic_head::set_interface (Score_element*me)
{
  me->set_interface (ly_symbol2scm ("rhythmic-head-interface"));
}

bool
Rhythmic_head::has_interface (Score_element*me)
{
  return me &&  me->has_interface (ly_symbol2scm ("rhythmic-head-interface"));
}
