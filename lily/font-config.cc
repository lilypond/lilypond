/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "font-config.hh"

#include "international.hh"
#include "main.hh"
#include "warn.hh"

#include <fontconfig/fontconfig.h>

// This routine creates a Fontconfig configuration object that LilyPond can use
// (through Pango) to find fonts.  LilyPond's datadir is always added as an
// application font directory (i.e., directory for custom, non-system fonts),
// because that is where Emmentaler is found, and also the default text fonts
// (URW++) in the official binaries.  If emmentaler is false, system font
// directories are searched too.  On the other hand, if emmentaler is true, the
// datadir is the only directory; this is because some Linux distributions
// install LilyPond's Emmentaler font in system font directories, which could
// result in LilyPond picking its font not from its datadir, but from the distro
// package, which might be a different version, while the Emmentaler font is
// developed together with LilyPond and should always have the same version.

unique_fcconfig_ptr
make_font_config (bool emmentaler)
{
  debug_output (_f ("Creating %sfont configuration...", emmentaler ? "Emmentaler " : ""));

  /* Create an empty configuration */
  unique_fcconfig_ptr conf (FcConfigCreate ());

  /* fontconfig conf files */
  std::vector<std::string> confs;

  if (emmentaler)
    {
      confs.push_back (lilypond_datadir + "/fonts/emmentaler-font.conf");
    }
  else
    {
      /* LilyPond local fontconfig conf file 00
         This file is loaded *before* fontconfig's default conf. */
      confs.push_back (lilypond_datadir + "/fonts/00-lilypond-fonts.conf");

      /* fontconfig's default conf file */
      {
        FcChar8 *default_conf = FcConfigFilename (nullptr);
        /* emplace_back only if default conf exists */
        if (default_conf)
          confs.emplace_back (reinterpret_cast<char *> (default_conf));
        else
          warning (_ ("cannot find fontconfig default config, skipping."));
        FcStrFree (default_conf);
      }

      /* LilyPond local fontconfig conf file 99
         This file is loaded *after* fontconfig's default conf. */
      confs.push_back (lilypond_datadir + "/fonts/99-lilypond-fonts.conf");
    }

  /* Load fontconfig conf files */
  for (const auto &conf_file : confs)
    {
      auto *const fcstr
        = reinterpret_cast<const FcChar8 *> (conf_file.c_str ());
      if (!FcConfigParseAndLoad (conf.get (), fcstr, FcTrue))
        error (_f ("failed to add fontconfig configuration file `%s'",
                   conf_file.c_str ()));
      else
        debug_output (
          _f ("Adding fontconfig configuration file: %s", conf_file.c_str ()));
    }

  std::string dir (lilypond_datadir + "/fonts/otf");

  if (!FcConfigAppFontAddDir (conf.get (),
                              reinterpret_cast<const FcChar8 *> (dir.c_str ())))
    error (_f ("failed adding font directory: %s", dir.c_str ()));
  else
    debug_output (_f ("Adding font directory: %s", dir.c_str ()));

  FcStrList *cache_dirs = FcConfigGetCacheDirs (conf.get ());
  FcChar8 *cache_dir;
  while ((cache_dir = FcStrListNext (cache_dirs)))
    debug_output (_f ("Font cache directory: %s\n", reinterpret_cast<char *> (cache_dir)));
  FcStrListDone (cache_dirs);

  debug_output (_ ("Building font database..."));
  // FcConfigParseAndLoad calls should be followed by FcConfigBuildFonts, which
  // does the actual work of building the font database using all the
  // configuration files loaded.  Note that adding extra configuration after
  // this function is called has indeterminate effect according to its
  // documentation, but adding application fonts is fine, and this is what
  // ly:font-config-add-{font,directory} do.
  FcConfigBuildFonts (conf.get ());

  debug_output ("\n");

  return conf;
}
