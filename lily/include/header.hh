/*
  header.hh -- declare Header

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef HEADER_HH
#define HEADER_HH

#include "string.hh"
#include "dictionary.hh"


/** Store bibliographical information.  The information in the \header
   block is read into this struct.  Lily does not process this
   information.  */
struct Header : Dictionary<String>
{
  String lily_id_str_;
  String TeX_string() const;
  Header ();
};

#endif // HEADER_HH
