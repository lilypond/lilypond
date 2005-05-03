/*
  font-config.cc -- implement FontConfig related functions

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "config.hh"

#if HAVE_FONTCONFIG

#include <fontconfig/fontconfig.h>
#include <sys/stat.h>
#include "file-path.hh"
#include "main.hh"
#include "warn.hh"

void
init_fontconfig ()
{
  if (be_verbose_global)
    message (_ ("Initializing FontConfig..."));
  
  if (!FcInit ())
    error (_ ("initializing FontConfig"));

  FcConfig *fcc = FcConfigGetCurrent ();
  Array<String> dirs;
  struct stat statbuf; 
  String builddir = prefix_directory + "/mf/out/";

  if (stat (builddir.to_str0 (), &statbuf) == 0)
    dirs.push (builddir.to_str0 ());
  else
    {
      dirs.push (prefix_directory + "/fonts/otf/");
      dirs.push (prefix_directory + "/fonts/type1/");
      dirs.push (prefix_directory + "/fonts/svg/");
    }
  
  for (int i = 0; i < dirs.size (); i++)
    {
      String dir = dirs[i];
      if (!FcConfigAppFontAddDir (fcc, (FcChar8 *)dir.to_str0 ()))
	error (_f ("adding font directory: %s", dir.to_str0 ()));
      else if (be_verbose_global)
	message (_f ("adding font directory: %s", dir.to_str0 ()));
    }

  if (be_verbose_global)
    progress_indication ("\n");
}

#else

void
init_fontconfig ()
{
  
}

#endif
