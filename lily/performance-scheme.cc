/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2019 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "performance.hh"

LY_DEFINE (ly_performance_header, "ly:performance-header",
           1, 0, 0, (SCM performance),
           "Return header of performance.")
{
  LY_ASSERT_SMOB (Performance, performance, 1);
  Performance *p = unsmob<Performance> (performance);
  return p->get_header ();
}

LY_DEFINE (ly_performance_set_header_x, "ly:performance-set-header!",
           2, 0, 0, (SCM performance, SCM module),
           "Set the performance header.")
{
  LY_ASSERT_SMOB (Performance, performance, 1);
  SCM_ASSERT_TYPE (ly_is_module (module), module, SCM_ARG2, __FUNCTION__,
                   "module");

  Performance *p = unsmob<Performance> (performance);
  p->set_header (module);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_performance_write, "ly:performance-write",
           3, 0, 0, (SCM performance, SCM filename, SCM name),
           "Write @var{performance} to @var{filename} storing @var{name} as "
           "the name of the performance in the file metadata.")
{
  LY_ASSERT_SMOB (Performance, performance, 1);
  LY_ASSERT_TYPE (scm_is_string, filename, 2);
  LY_ASSERT_TYPE (scm_is_string, name, 3);

  unsmob<Performance> (performance)->write_output (ly_scm2string (filename),
                                                   ly_scm2string (name));
  return SCM_UNSPECIFIED;
}
