/*
  freetype.cc -- implement Freetype routines.

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "freetype.hh"
#include "warn.hh"

FT_Library freetype2_library;

void
init_freetype ()
{
  int errorcode = FT_Init_FreeType (&freetype2_library);
  if (errorcode)
    error ("cannot initialize FreeType");
}

