/*
  change-translator.cc -- implement Change_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "change-translator.hh"
#include "debug.hh"



void
Change_translator::do_print () const
{
  DEBUG_OUT << "change " << change_to_type_str_ << " to " << change_to_id_str_ ;
}
  
