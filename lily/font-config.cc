/*
  font-config.cc -- implement Font_config related functions

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "config.hh"

#ifdef HAVE_PANGO_FT2

#include <fontconfig/fontconfig.h>

#include "main.hh"
#include "warn.hh"

void
init_fontconfig ()
{
  if (!FcInit())
    {
      error ("FontConfig failed to initialize"); 
    }

  char const **dirs = prefix_directories;
  for (; *dirs; dirs++)
    {
      String path = String (*dirs) + "/" + "otf";
      
      if (!FcConfigAppFontAddDir (0, (FcChar8*)path.to_str0 ()))
	{
	  error (_f ("Failed to add lilypond directory %s", path.to_str0 ()));
	}
    }     
}

#else

void init_fontconfig() {}
#endif
