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


Dots*
Rhythmic_head::dots_l () const
{
  SCM s = get_elt_pointer ("dot");
  return dynamic_cast<Dots*> (unsmob_element (s));
}

int
Rhythmic_head::balltype_i () const
{
  SCM s = get_elt_property ("duration-log");
  
  return gh_number_p (s) ? gh_scm2int (s) : 0;
}

Stem*
Rhythmic_head::stem_l () const
{
  SCM s = get_elt_pointer ("stem");
  return dynamic_cast<Stem*> (unsmob_element (s));
}

int
Rhythmic_head::dot_count () const
{
  return dots_l ()
    ? gh_scm2int (dots_l ()->get_elt_property ("dot-count")) : 0;
}
  
GLUE_SCORE_ELEMENT(Rhythmic_head,after_line_breaking);
SCM
Rhythmic_head::member_after_line_breaking ()
{
  if (Dots *d = dots_l ())
    {
      Staff_symbol_referencer_interface si (d);
      Staff_symbol_referencer_interface me (d);      
      si.set_position(int (me.position_f ()));
    }

  return SCM_UNDEFINED;
}


void
Rhythmic_head::add_dots (Dots *dot_l)
{
  set_elt_pointer ("dot", dot_l->self_scm_);
  dot_l->add_dependency (this);  
}


Rhythmic_head::Rhythmic_head (SCM s)
  : Item (s)
{
}
