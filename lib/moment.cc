/*
  moment.cc -- implement Moment

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <iostream.h>
#include "string.hh"
#include "moment.hh"

void
printm (Moment const &m)
{
    cout << String (m) << flush;
}
    

