/*
  header.hh -- declare Header

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef HEADER_HH
#define HEADER_HH

#include "string.hh"
#include "dictionary.hh"
#include "scope.hh"


/** Store bibliographical information.  The information in the \header
   block is read into this struct.  Lily does not process this
   information.  */
typedef Scope Header;

#endif // HEADER_HH
