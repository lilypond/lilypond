/*
  header.cc -- implement Header

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "string.hh"
#include "dictionary-iter.hh"
#include "header.hh"
#include "main.hh"

extern char const *lily_version_number_sz();


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

Header::Header ()
{
  lily_id_str_ = "Lily was here, " +
    String (lily_version_number_sz ());
}
