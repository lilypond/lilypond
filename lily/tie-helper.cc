/*
  tie-helper.cc -- implement Tie_configuration, Tie_details

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "tie.hh"
#include "bezier.hh"
#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"
#include "tie-formatting-problem.hh"

void
Tie_details::from_grob (Grob *me)
{
  staff_symbol_referencer_ = me;
  staff_space_ = Staff_symbol_referencer::staff_space (me);
  SCM details = me->get_property ("details");

  height_limit_ = robust_scm2double (ly_assoc_get (ly_symbol2scm ("height-limit"), details, SCM_EOL),
				     0.75) * staff_space_;
  
  ratio_ = robust_scm2double (ly_assoc_get (ly_symbol2scm ("ratio"), details, SCM_EOL),
			      .333);
  
  x_gap_ = robust_scm2double (me->get_property ("x-gap"), 0.2);
  between_length_limit_
    = robust_scm2double (ly_assoc_get (ly_symbol2scm ("between-length-limit"), details, SCM_EOL),
			 1.0); 
  
}

Tie_details::Tie_details ()
{
  staff_space_ = 1.0; 
  height_limit_ = 1.0;
  ratio_ = .333;   
}

