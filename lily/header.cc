/*
  header.cc -- implement Header

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "string.hh"
#include "dictionary-iter.hh"
#include "header.hh"
#include "main.hh"

extern char const *lily_version_number_sz ();

Header::Header ()
{
  lily_id_str_ = "Lily was here, " +
    String (lily_version_number_sz ());
}

//urg
String
Header::tex_string () const
{
  String s;

  s+= "\\def\\LilyIdString{"  + lily_id_str_ + "}";
  
  for (Dictionary_iter<String> i (*this); i.ok (); i++) 
    s += "\\def\\mudela" + i.key () + "{" + i.val () + "}";
  return s;
}

String
Header::ps_string () const
{
  String s;

  s+= "/lily_id_string\n{" + lily_id_str_ + "} bind def\n";
  
  for (Dictionary_iter<String> i (*this); i.ok (); i++) 
    s += "/mudela" + i.key () + "{" + i.val () + "} bind def";
  return s;
}

