/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "warn.hh"

#include <cstdlib>
#include <cstdio>

#include "international.hh"

using namespace std;

/* Is progress indication at NEWLINE?  */
static bool progress_newline = true;

/* Display user information that is not a full message.  */
void
progress_indication (string s)
{
  /* Test if all silly progress_indication ("\n") can be dropped now.  */
  if (s == "\n")
    return;

  fputs (s.c_str (), stderr);
  fflush (stderr);
  if (s.length ())
    progress_newline = s[s.length () - 1] == '\n';
}

/* Display a single user message.  Always starts on a new line.  */
void
message (string s)
{
  if (!progress_newline)
    fputc ('\n', stderr);
  progress_indication (s);
}

/* Display a warning message.  Always starts on a new line.  */
void
warning (string s)
{
  message (_f ("warning: %s", s.c_str ()) + "\n");
}

void
non_fatal_error (string s)
{
  message (_f ("error: %s", s.c_str ()) + "\n");
}

/* Display an error message.  Always starts on a new line.  */
void
error (string s)
{
  non_fatal_error (s);
  exit (1);
}

void
programming_error (string s)
{
  message (_f ("programming error: %s", s) + "\n");
  message (_ ("continuing, cross fingers") + "\n");
}

