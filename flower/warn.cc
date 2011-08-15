/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

/** We have several different loglevels, each with its own message function(s):
      ERROR: error, non_fatal_error, programming_error
      WARN: warning
      BASIC_PROGRESS: success/...
      PROGRESS: progress_indication
      INFO: message
      DEBUG: debug
  All these functions check whether the corresponding loglevel bit is set
  and print the message only if that's the case
*/

/* Define the loglevel (default is PROGRESS); for now, PROGRESS=INFO for a
   all relevant output, so be on the safe side and use INFO as default, just
   in case some output is generated with INFO */
int loglevel = LOGLEVEL_INFO;

bool
is_loglevel (int level)
{
  // Check the bitmask containing the loglevel
  return (loglevel & level);
}

void
set_loglevel (int level)
{
  loglevel = level;
  debug_output (_f ("Log level set to %d\n", loglevel));
}

void
set_loglevel (string level)
{
  /* Convert the loglevel string to lower-case, so we allow
     both upper- and lower-case loglevels */
  std::transform (level.begin (), level.end (), level.begin (), ::tolower);

  /* Compare just the first few characters, so the loglevels
     can be abbreviated */
  if (level.compare (0, 5, "debug") == 0) // debug
    set_loglevel (LOGLEVEL_DEBUG);
  else if (level.compare (0, 4, "info") == 0) // info
    set_loglevel (LOGLEVEL_INFO);
  else if (level.compare (0, 4, "prog") == 0) // progress
    set_loglevel (LOGLEVEL_PROGRESS);
  else if (level.compare (0, 5, "basic") == 0) // basic progress
    set_loglevel (LOGLEVEL_BASIC);
  else if (level.compare (0, 4, "warn") == 0) // warning
    set_loglevel (LOGLEVEL_WARN);
  else if (level.compare (0, 3, "err") == 0) // error
    set_loglevel (LOGLEVEL_ERROR);
  else if (level.compare (0, 4, "none") == 0) // none
    set_loglevel (LOGLEVEL_NONE);
  else
    {
      int l;
      if (sscanf (level.c_str (), "%d", &l))
        set_loglevel (l);
      else
        {
          non_fatal_error (_f ("unknown log level `%s', using default (PROGRESS)", 
                               level));
          set_loglevel (LOGLEVEL_INFO);
        }
    }
}


/**
 * Helper functions: print_message_part (no newline prepended)
 *                   print_message (always starts on a new line)
 */

/* Is output message at NEWLINE?  */
static bool message_newline = true;

/* Display user information as a full message.
   if newline is true, start the message on a new line.
*/
void
print_message (int level, string location, string s, bool newline)
{
  /* Only print the message if the current loglevel allows it: */
  if (!is_loglevel (level))
    return;
  if (newline && !message_newline)
    fputc ('\n', stderr);

  /* Test if all silly progress_indication ("\n") can be dropped now.  */
  if (s == "\n")
    return;

  if (!location.empty ())
    s = location + ": " + s;
  fputs (s.c_str (), stderr);
  fflush (stderr);
  if (s.length ())
    message_newline = s[s.length () - 1] == '\n';
}


/** The actual output functions to be called in lilypond code.
 *  Sorted in descending order of importance (errors, warnings, progress, info,
 *  debug). Each prints a message on a separate line.
 */

/* Display a fatal error message.  Also exits lilypond.  */
void
error (string s, string location)
{
  print_message (LOG_ERROR, location, _f ("fatal error: %s", s) + "\n");
  exit (1);
}

/* Display a severe programming error message, but don't exit.  */
void
programming_error (string s, string location)
{
  print_message (LOG_ERROR, location, _f ("programming error: %s", s) + "\n");
  print_message (LOG_ERROR, location, _ ("continuing, cross fingers") + "\n");
}

/* Display a non-fatal error message, don't exit.  */
void
non_fatal_error (string s, string location)
{
  print_message (LOG_ERROR, location, _f ("error: %s", s) + "\n");
}

/* Display a warning message. */
void
warning (string s, string location)
{
  print_message (LOG_WARN, location, _f ("warning: %s", s) + "\n");
}

/* Display a success message.  */
void
successful (string s, string location)
{
  print_message (LOG_BASIC, location, _f ("success: %s", s) + "\n", true);
}

/* Display information about the progress.  */
void
progress_indication (string s, bool newline, string location)
{
  print_message (LOG_PROGRESS, location, s, newline);
}

/* Display a single info message.  */
void
message (string s, bool newline, string location)
{
  // Use the progress loglevel for all normal messages (including progress msg)
  print_message (LOG_INFO, location, s, newline);
}

/* Display a debug information, not necessarily on a new line.  */
void
debug_output (string s, bool newline, string location)
{
  print_message (LOG_DEBUG, location, s, newline);
}
