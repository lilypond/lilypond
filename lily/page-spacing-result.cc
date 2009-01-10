/* 
  page-spacing-result.cc -- implement Page_spacing_result
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2007--2009 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "page-spacing-result.hh"
#include <cstdio>

Page_spacing_result::Page_spacing_result ()
{
  penalty_ = 0;
  demerits_ = infinity_f;
}

vsize
Page_spacing_result::system_count () const
{
  vsize total = 0;
  for (vsize i = 0; i < systems_per_page_.size(); i++)
    total += systems_per_page_[i];

  return total;      
}

vsize
Page_spacing_result::page_count () const
{
  return systems_per_page_.size();
}

Real
Page_spacing_result::average_force () const
{
  Real average_force = 0;
  for (vsize i = 0; i < page_count (); i++)
    average_force += force_[i];

  average_force /= page_count ();
  return average_force;
}

void
Page_spacing_result::print () const
{
  printf ("penalty %lf, demerits %lf\n" , penalty_, demerits_);
  for (vsize i = 0; i < page_count (); i++)
    printf (" %d:  #sys=%d, force=%lf\n", int (i), int (systems_per_page_[i]),
	    force_[i]);
}
