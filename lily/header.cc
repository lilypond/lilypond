/*
  header.cc -- implement Header

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "string.hh"
#include "assoc-iter.hh"
#include "header.hh"

String
Header::TeX_string() const
{
  String s;
  for (Assoc_iter<String, String> i(*this); i.ok(); i++) 
    {
      s += "\\def\\mudela" + i.key() + "{" + i.val() + "}";
    }
  return s;
}
