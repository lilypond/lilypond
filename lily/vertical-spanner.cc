/*
  vertical-spanner.cc -- implement Vertical_spanner

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "vertical-spanner.hh"

Vertical_spanner::Vertical_spanner()
{
    upper_pstaff_l_ = lower_pstaff_l_ = 0;
}

IMPLEMENT_STATIC_NAME(Vertical_spanner);
    
