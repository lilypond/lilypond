/*
  font-config.cc -- implement FontConfig related functions

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "config.hh"

#ifdef HAVE_FONTCONFIG

#include <fontconfig/fontconfig.h>

#include "file-path.hh"
#include "main.hh"
#include "warn.hh"

void
init_fontconfig ()
{
  if (be_verbose_global)
    progress (_("Initializing FontConfig ..."));
  
  if (!FcInit ())
    error (_ ("FontConfig failed to initialize"));

  FcConfig *fcc = FcConfigGetCurrent ();

  Array<String> dirs;
  dirs.push (prefix_directory + "/fonts/otf/");
  dirs.push (prefix_directory + "/mf/out/");
  dirs.push (prefix_directory + "/fonts/type1/");
  dirs.push (prefix_directory + "/fonts/cff/");

  for (int i = 0; i < dirs.size (); i++)
    {
      String dir = dirs[i];
      if (!FcConfigAppFontAddDir (fcc, (FcChar8 *)dir.to_str0 ()))
	error (_f ("Failed to add lilypond directory %s", dir));
    }

  if (be_verbose_global)
    progress ("\n");
}

#else

void
init_fontconfig ()
{
}
#endif
