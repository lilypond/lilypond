/*
  font-config.cc -- implement FontConfig related functions

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "config.hh"

#if HAVE_FONTCONFIG

#include <cstdio>
#include <fontconfig/fontconfig.h>
#include <sys/stat.h>

#include "file-path.hh"
#include "international.hh"
#include "main.hh"
#include "warn.hh"


FcConfig *font_config_global = 0;

void
init_fontconfig ()
{
  if (be_verbose_global)
    message (_ ("Initializing FontConfig..."));

  font_config_global = FcInitLoadConfig ();
  FcChar8 *cache_file = FcConfigGetCache (font_config_global);

#if 0
  // always returns 0 for FC 2.4
  if (!cache_file)
    programming_error ("Cannot find file for FontConfig cache.");
#endif
  /*
    This is a terrible kludge, but there is apparently no way for
    FontConfig to signal whether it needs to rescan directories.
   */ 
  if (cache_file
      && !is_file ((char const *)cache_file))
    message (_f ("Rebuilding FontConfig cache %s, this may take a while...", cache_file));
			
  vector<string> dirs;

  /* Extra trailing slash suddenly breaks fontconfig (fc-cache 2.5.0)
     on windows.  */
  dirs.push_back (lilypond_datadir + "/fonts/otf");
  dirs.push_back (lilypond_datadir + "/fonts/type1");
  
  for (vsize i = 0; i < dirs.size (); i++)
    {
      string dir = dirs[i];
      if (!FcConfigAppFontAddDir (font_config_global, (FcChar8 *)dir.c_str ()))
	error (_f ("failed adding font directory: %s", dir.c_str ()));
      else if (be_verbose_global)
	message (_f ("adding font directory: %s", dir.c_str ()));
    }
  
  if (be_verbose_global)
    message (_ ("Building font database."));
  FcConfigBuildFonts (font_config_global);
  FcConfigSetCurrent (font_config_global);
  if (be_verbose_global)
    message ("\n");

  if (cache_file
      && !is_file ((char*)cache_file))
    {
      /* inhibit future messages. */
      FILE *f = fopen ((char*)cache_file, "w");
      if (f)
	fclose (f);
    }
  
}

#else

void
init_fontconfig ()
{
}

#endif
