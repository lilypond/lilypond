/*
  header.hh -- declare 

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HEADER_HH
#define HEADER_HH

#include "string.hh"
#include "dictionary.hh"


struct Header : Dictionary<String>
{
  String lily_id_str_;
  String TeX_string() const;
};

#endif // HEADER_HH
