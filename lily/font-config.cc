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
#include "file-path.hh"

void
init_fontconfig ()
{
  if (!FcInit())
    {
      error ("FontConfig failed to initialize"); 
    }

  FcConfig *fcc = FcConfigGetCurrent ();

  
  Array<String> dirs;
  dirs.push (prefix_directory + "/otf/");
  dirs.push (prefix_directory + "/mf/out/");
  dirs.push (prefix_directory + "/type1/");
  dirs.push (prefix_directory + "/cff/");
  
  for (int i = 0; i < dirs.size(); i++)
    {
      String path = dirs[i];
      if (!FcConfigAppFontAddDir (fcc, (FcChar8*)path.to_str0 ()))
	{
	  error (_f ("Failed to add lilypond directory %s", path.to_str0 ()));
	}
    }     
}

#else

void init_fontconfig() {}
#endif
