/*
  header.cc -- implement Header

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "string.hh"
#include "dictionary-iter.hh"
#include "header.hh"

String
Header::TeX_string() const
{
  String s;

  s+= "\\def\\LilyIdString{"  + lily_id_str_ + "}";
  
  for (Dictionary_iter<String> i(*this); i.ok(); i++) 
    {
      s += "\\def\\mudela" + i.key() + "{" + i.val() + "}";
    }
  return s;
}
