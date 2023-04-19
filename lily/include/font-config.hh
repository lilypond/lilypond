/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2023--2023 Jean Abou Samra <jean@abou-samra.fr>

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

#ifndef FONT_CONFIG_HH
#define FONT_CONFIG_HH

#include "memory.hh"

#include <fontconfig/fontconfig.h>

using unique_fcconfig_ptr = unique_ptr_with_freer<FcConfig, FcConfigDestroy>;
unique_fcconfig_ptr make_font_config (bool emmentaler);

#endif
