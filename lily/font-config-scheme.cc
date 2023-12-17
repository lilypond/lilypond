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

#include "all-font-metrics.hh"
#include "lily-guile.hh"
#include "warn.hh"

LY_DEFINE (ly_font_config_get_font_file, "ly:font-config-get-font-file", 1, 0,
           0, (SCM name),
           R"(
Get the file for font @var{name}, as found by FontConfig.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);
  std::string n = ly_scm2string (name);
  return ly_string2scm (all_fonts_global->get_font_file (n));
}

LY_DEFINE (ly_font_config_display_fonts, "ly:font-config-display-fonts", 0, 1,
           0, (SCM port),
           R"(
List all fonts visible to FontConfig, together with directory information.

Optional argument @var{port} selects the output port; the default is
@code{(current-error-port)}.
           )")
{
  SCM use_port = scm_current_error_port ();
  if (!SCM_UNBNDP (port))
    {
      LY_ASSERT_TYPE (ly_is_port, port, 1)
      use_port = port;
    }
  all_fonts_global->display_fonts (use_port);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_font_config_add_directory, "ly:font-config-add-directory", 1, 0,
           0, (SCM dir),
           R"(
Add directory @var{dir} to FontConfig.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, dir, 1);
  std::string d = ly_scm2string (dir);
  all_fonts_global->add_font_directory (d);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_font_config_add_font, "ly:font-config-add-font", 1, 0, 0,
           (SCM font),
           R"(
Add font @var{font} to FontConfig.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, font, 1);
  std::string f = ly_scm2string (font);
  all_fonts_global->add_font_file (f);
  return SCM_UNSPECIFIED;
}
