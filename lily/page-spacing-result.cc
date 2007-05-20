/* 
  page-spacing-result.cc -- implement Page_spacing_result
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2007 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "page-spacing-result.hh"

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
