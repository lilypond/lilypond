/*
  notename-table.cc -- implement Notename_table

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "notename-table.hh"
#include "pointer.tcc"
#include "musical-request.hh"
#include "dictionary-iter.hh"

String
Notename_table::get_name (Musical_pitch m) const
{

  for (Dictionary_iter<Musical_pitch> ai (*this); ai.ok (); ai++)
    {
      if (ai.val () == m)
	return ai.key ();
    }
  return "r";			// rest. 
}
