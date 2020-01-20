/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef WARN_HH
#define WARN_HH

#include "std-string.hh"

/* Log-level bitmasks */
#define LOG_NONE 0
#define LOG_ERROR 1<<0
#define LOG_WARN 1<<1
#define LOG_BASIC 1<<2  // undocumented basic_progress, i.e. input file name and success
#define LOG_PROGRESS 1<<3
#define LOG_INFO 1<<4
#define LOG_DEBUG 1<<8

/* Log-level definitions (or'ed bitmasks) */
#define LOGLEVEL_NONE (LOG_NONE)
#define LOGLEVEL_ERROR (LOG_ERROR)
#define LOGLEVEL_WARN (LOGLEVEL_ERROR | LOG_WARN)
#define LOGLEVEL_BASIC (LOGLEVEL_WARN | LOG_BASIC)
#define LOGLEVEL_PROGRESS (LOGLEVEL_BASIC | LOG_PROGRESS)
#define LOGLEVEL_INFO (LOGLEVEL_PROGRESS | LOG_INFO)
#define LOGLEVEL_DEBUG (LOGLEVEL_INFO | LOG_DEBUG)

extern int loglevel;
extern bool warning_as_error;

/* output messages, in decreasing order of importance */
void error (string s, const string &location = ""); // Fatal error, exits lilypond!
void programming_error (const string &s, const string &location = "");
void non_fatal_error (const string&, const string &location = "");
void warning (const string &s, const string &location = "");
void basic_progress (const string &s, const string &location = "");
/* progress_indication does by default *NOT* start on a new line */
void progress_indication (const string &s, bool newline = false, const string &location = "");
void message (const string &s, bool newline = true, const string &location = "");
void debug_output (const string &s, bool newline = true, const string &location = "");

/* Helper functions that always print out the message. Callers should ensure
   that the loglevel is obeyed */
void print_message (int level, const string &location, string s, bool newline = true);

bool is_loglevel (int level);
void set_loglevel (int level);
void set_loglevel (string level);

void expect_warning (const string &msg);
void check_expected_warnings ();

// The following templates accept any kind of object in place of the location
// parameter.  All that is required to use this feature is to overload
// source_location (const Whatever &X), returning the location string from X.

// Automatically follow a pointer looking for the source location.
template <class Bearer>
inline string source_location (const Bearer *bearer)
{
  return bearer ? source_location (*bearer) : string ();
}

template <class LocationBearer>
inline void error (const string &s, const LocationBearer &location_bearer)
{
  error (s, source_location (location_bearer));
}

template <class LocationBearer>
inline void programming_error (const string &s,
                               const LocationBearer &location_bearer)
{
  programming_error (s, source_location (location_bearer));
}

template <class LocationBearer>
inline void non_fatal_error (const string &s,
                             const LocationBearer &location_bearer)
{
  non_fatal_error (s, source_location (location_bearer));
}

template <class LocationBearer>
inline void warning (const string &s,
                     const LocationBearer &location_bearer)
{
  warning (s, source_location (location_bearer));
}

template <class LocationBearer>
inline void basic_progress (const string &s,
                            const LocationBearer &location_bearer)
{
  basic_progress (s, source_location (location_bearer));
}

template <class LocationBearer>
inline void progress_indication (const string &s, bool newline,
                                 const LocationBearer &location_bearer)
{
  progress_indication (s, newline, source_location (location_bearer));
}

template <class LocationBearer>
inline void message (const string &s, bool newline,
                     const LocationBearer &location_bearer)
{
  message (s, newline, source_location (location_bearer));
}

template <class LocationBearer>
inline void debug_output (const string &s, bool newline,
                          const LocationBearer &location_bearer)
{
  debug_output (s, newline, source_location (location_bearer));
}

#endif /* WARN_HH */
