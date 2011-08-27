/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
  debug_output (_ ("Initializing FontConfig..."));

  /* TODO: Find a way for Fontconfig to update its cache, if needed. */
  font_config_global = FcInitLoadConfig ();

  vector<string> dirs;

  /* Extra trailing slash suddenly breaks fontconfig (fc-cache 2.5.0)
     on windows.  */
  dirs.push_back (lilypond_datadir + "/fonts/otf");

  for (vsize i = 0; i < dirs.size (); i++)
    {
      string dir = dirs[i];
      if (!FcConfigAppFontAddDir (font_config_global, (FcChar8 *)dir.c_str ()))
        error (_f ("failed adding font directory: %s", dir.c_str ()));
      else
        debug_output (_f ("Adding font directory: %s", dir.c_str ()));
    }

  debug_output (_ ("Building font database..."));

  FcConfigBuildFonts (font_config_global);
  FcConfigSetCurrent (font_config_global);

  debug_output ("\n");

}

#else

void
init_fontconfig ()
{
}

#endif
