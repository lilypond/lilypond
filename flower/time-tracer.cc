/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2024 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "time-tracer.hh"

#include "international.hh"
#include "string-convert.hh"
#include "warn.hh"

#include <cerrno>
#include <chrono>
#include <cstdint>
#include <cstdio>
#include <string>
#include <string_view>
#include <type_traits>
#include <unistd.h>

using namespace std::literals;

static FILE *
to_FILE (void *f)
{
  return static_cast<FILE *> (f);
}

static auto
to_microsecond_count (Time_trace_clock::time_point t)
{
  return std::chrono::floor<std::chrono::microseconds> (t.time_since_epoch ())
    .count ();
}

void
Time_tracer::trace (const char *format, ...)
{
  va_list args;
  va_start (args, format);
  if (file_)
    {
      if (vfprintf (to_FILE (file_), format, args) >= 0)
        {
          // it worked
        }
      else // [[unlikely]]
        {
          error (_i ("failed to write to time-trace file"));
        }
    }
  else
    {
      buffered_output_ += String_convert::vform_string (format, args);
    }
  va_end (args);
}

void
Time_tracer::disable ()
{
  if (!enabled_) // unexpected, but not a problem
    {
      return;
    }

  if (file_)
    {
      programming_error ("cannot disable Time_tracer after starting output");
      return;
    }

  enabled_ = false;
  buffered_output_.clear ();
}

bool
Time_tracer::set_file (const char *output_file_name)
{
  if (!enabled_)
    {
      programming_error ("cannot enable Time_tracer after disabling");
      return false;
    }

  if (file_)
    {
      programming_error ("cannot change Time_tracer output file");
      return false;
    }

  file_ = fopen (output_file_name, "w");
  if (!file_)
    {
      return false;
    }

  if (!buffered_output_.empty ())
    {
      if (fwrite (buffered_output_.data (), sizeof (char),
                  buffered_output_.size (), to_FILE (file_))
          != buffered_output_.size ())
        {
          error (_i ("failed to write to time-trace file"));
        }
      buffered_output_.clear ();
    }

  return true;
}

bool
Time_tracer::include_file (const char *included_file_name)
{
  if (!enabled_)
    {
      // How would we even get a file to insert if tracing were disabled?
      programming_error (
        "cannot include time-trace file with tracing disabled");
      return false;
    }

  FILE *included_file = nullptr;
  unsigned long end_pos = 0;

  if (!file_)
    {
      log_instant_event (String_convert::form_string (
        "unwilling to buffer included time-trace file %s in memory; skipping",
        included_file_name));
      goto return_error;
    }

  included_file = fopen (included_file_name, "r");
  if (!included_file)
    {
      const auto err = errno;
      log_instant_event (String_convert::form_string (
        "failed to open included time-trace file %s (errno=%d); skipping",
        included_file_name, err));
      goto return_error;
    }

  // Does the input look complete?
  if (fseek (included_file, -3, SEEK_END) != 0)
    {
      const auto err = errno;
      log_instant_event (String_convert::form_string (
        "failed to seek in included time-trace file %s (errno=%d); skipping",
        included_file_name, err));
      goto return_error;
    }

  end_pos = ftell (included_file);

  if ((getc (included_file) != '\n') || (getc (included_file) != ']')
      || (getc (included_file) != '\n'))
    {
      log_instant_event (String_convert::form_string (
        "included time-trace file %s seems incomplete; skipping",
        included_file_name));
      goto return_error;
    }

  if (fseek (included_file, 0, SEEK_SET) != 0)
    {
      const auto err = errno;
      log_instant_event (
        String_convert::form_string ("failed to rewind in included time-trace "
                                     "file %s (errno=%d); skipping",
                                     included_file_name, err));
      goto return_error;
    }

  if ((getc (included_file) != '[') || (getc (included_file) != '\n'))
    {
      log_instant_event (String_convert::form_string (
        "included time-trace file %s seems corrupt; skipping",
        included_file_name));
      goto return_error;
    }

  // copy the file
  {
    char buf[4096];
    auto remainder = size_t (end_pos - ftell (included_file));
    while (remainder > 0)
      {
        const auto size = (remainder >= std::size (buf)) ? std::size (buf)
                                                         : size_t (remainder);
        if (fread (buf, sizeof (char), size, included_file) != size)
          {
            log_instant_event (String_convert::form_string (
              "error reading included time-trace file %s",
              included_file_name));
            goto return_error;
          }
        if (fwrite (buf, sizeof (char), size, to_FILE (file_)) != size)
          {
            error (_i ("failed to write to time-trace file"));
          }
        remainder -= size;
      }

    // The final event of the included file would not have been followed by a
    // comma, so add one.
    if ((putc (',', to_FILE (file_)) != ',')
        || putc ('\n', to_FILE (file_)) != '\n')
      {
        log_instant_event (String_convert::form_string (
          "error writing time-trace data included from %s",
          included_file_name));
        goto return_error;
      }
  }

  fclose (included_file);
  return true;

return_error:
  if (included_file)
    {
      fclose (included_file);
    }
  return false;
}

void
Time_tracer::log_begin_event (std::string_view prefix, std::string_view name,
                              std::string_view arg)
{
  const auto start_us = to_microsecond_count (Time_trace_clock::now ());

  if (!arg.empty ())
    {
      trace ("%*s{"
             R"("ts":%jd,)"
             R"("pid":%d,)"
             R"("tid":0,)"
             R"("ph":"B",)"
             R"("name":"%*s",)"
             R"("args":{)"
             /**/ R"("arg":"%*s")"
             "}"
             "},\n",
             static_cast<int> (prefix.size ()), prefix.data (),
             intmax_t {start_us}, pid_, static_cast<int> (name.size ()),
             name.data (), static_cast<int> (arg.size ()), arg.data ());
    }
  else
    {
      trace ("%*s{"
             R"("ts":%jd,)"
             R"("pid":%d,)"
             R"("tid":0,)"
             R"("ph":"B",)"
             R"("name":"%*s")"
             "},\n",
             static_cast<int> (prefix.size ()), prefix.data (),
             intmax_t {start_us}, pid_, static_cast<int> (name.size ()),
             name.data ());
    }
}

void
Time_tracer::log_end_event (std::string_view arg, std::string_view suffix)
{
  // Idea: To reduce the size of the trace file, if no duration event was nested
  // inside the one that it ending, emit an "X" event instead of a "B" and "E"
  // events.  This would require delaying the "B" until it is known whether it
  // needs to be produced.  It might not be worth the trouble.

  // Idea: Track the nesting depth and abort if this End Event is unexpected.
  // It would be a programming error.

  const auto end_us = to_microsecond_count (Time_trace_clock::now ());

  if (!arg.empty ())
    {
      trace ("{"
             R"("ts":%jd,)"
             R"("pid":%d,)"
             R"("tid":0,)"
             R"("ph":"E",)"
             R"("args":{)"
             /**/ R"("arg":"%*s")"
             "}"
             "}%*s\n",
             intmax_t {end_us}, pid_, static_cast<int> (arg.size ()),
             arg.data (), static_cast<int> (suffix.size ()), suffix.data ());
    }
  else
    {
      trace ("{"
             R"("ts":%jd,)"
             R"("pid":%d,)"
             R"("tid":0,)"
             R"("ph":"E")"
             "}%*s\n",
             intmax_t {end_us}, pid_, static_cast<int> (suffix.size ()),
             suffix.data ());
    }
}

void
Time_tracer::log_first_begin_event (std::string_view name)
{
  if (!enabled_)
    {
      return;
    }

  if (!pid_) // actual first-time call
    {
      // int should be sufficient, but if this detects narrowing on some
      // platform, try switching to intmax_t and printing with "%jd".
      pid_ = decltype (pid_) {getpid ()};
    }
  else if (const auto pid = getpid (); pid_ != pid) // child after forking
    {
      // dissociate from the parent's state
      pid_ = pid;
      if (file_)
        {
          fclose (to_FILE (file_));
          file_ = nullptr;
        }
      buffered_output_.clear ();
    }
  else
    {
      programming_error ("Time_tracer already logged first event");
      return;
    }
  log_begin_event ("[\n"sv, name, /*arg*/ {});
}

void
Time_tracer::log_begin_event (std::string_view name, std::string_view arg)
{
  if (!enabled_)
    {
      return;
    }

  log_begin_event (""sv, name, arg);
}

void
Time_tracer::log_begin_event (std::string_view name)
{
  if (!enabled_)
    {
      return;
    }

  log_begin_event (""sv, name, /* arg */ {});
}

void
Time_tracer::log_end_event (std::string_view arg)
{
  if (!enabled_)
    {
      return;
    }

  log_end_event (arg, ","sv);
}

void
Time_tracer::log_end_event ()
{
  if (!enabled_)
    {
      return;
    }

  log_end_event (/*arg*/ {}, ","sv);
}

void
Time_tracer::log_final_end_event (std::string_view arg)
{
  if (!enabled_)
    {
      return;
    }

  log_end_event (arg, "\n]"sv);
  pid_ = 0;
  if (file_)
    {
      fclose (to_FILE (file_));
      file_ = nullptr;
    }
  buffered_output_.clear ();
}

void
Time_tracer::log_instant_event (std::string_view name)
{
  if (!enabled_)
    {
      return;
    }

  const auto instant_us = to_microsecond_count (Time_trace_clock::now ());

  trace ("{"
         R"("ts":%jd,)"
         R"("pid":%d,)"
         R"("tid":0,)"
         R"("ph":"i",)"
         R"("name":"%*s",)"
         R"("s":"t")"
         "},\n",
         intmax_t {instant_us}, pid_, static_cast<int> (name.size ()),
         name.data ());
}
