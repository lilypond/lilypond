/*
  freetype.hh -- declare Freetype global settings.

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef FREETYPE_HH
#define FREETYPE_HH

#include <ft2build.h>
#include FT_FREETYPE_H

void init_freetype ();
extern FT_Library freetype2_library;

#endif /* FREETYPE_HH */

