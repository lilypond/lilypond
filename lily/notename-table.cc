/*
  notename-table.cc -- implement Notename_table

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "notename-table.hh"
#include "pointer.tcc"
#include "musical-request.hh"

template class P<Melodic_req>;

void
Notename_table::add (String s, Melodic_req *m_p)
{
  elem (s).set_p (m_p);
}

Melodic_req*
Notename_table::get_l (String s)
{
  if (! elt_b (s))
	return 0;
  return elem (s);
}
  
