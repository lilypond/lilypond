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

#ifndef TIME_TRACER_HH
#define TIME_TRACER_HH

#include <chrono>
#include <string>
#include <string_view>
#include <utility>

using Time_trace_clock = std::chrono::steady_clock;

// Time_tracer writes a performance log in Google's Trace Event Format, also
// known as Chrome tracing format.
//
// Time_tracer is ready to log immediately after construction: all entries are
// buffered in memory until `set_file` is called to configure the output or
// `disable` is called to disable tracing.
//
// The first and final logging calls must be `log_first_begin_event` and
// `log_final_end_event`.  After forking, a child process must call
// `log_first_begin_event` (again) to dissociate from the parent's tracing state
// and establish its own before logging any other events.
class Time_tracer
{
public:
  class [[nodiscard]] Scoped_slice;

private:
  // This stores logged content until either `set_file` or `disable` is called.
  std::string buffered_output_;
  // Think about inlining the checks of this flag for performance.  It doesn't
  // seem to be a problem right now.
  bool enabled_ = true;
  // This is set to the current process ID when the first event is logged.
  int pid_ = 0;
  // `void*` to save including stdio.h (can't forward-declare FILE)
  void *file_;

private:
  void log_begin_event (std::string_view prefix, std::string_view name,
                        std::string_view arg);
  void log_end_event (std::string_view arg, std::string_view suffix);
  void trace (const char *format, ...);

public:
  // Permanently disable tracing and discard buffered events.
  // This is an alternative to `set_file`.
  void disable ();

  // Open the named file, write all buffered events to it, and write future
  // events directly to it.
  bool set_file (const char *output_file_name);

  // Copy all entries from the named file into the current trace.
  bool include_file (const char *included_file_name);

  // Log the beginning of a Duration Event (type "B").  Duration events must be
  // properly paired and nested, so when possible, call `log_scope` rather than
  // `log_begin_event` and `log_end_event` to help avoid mistakes.
  //
  // Trace viewers can be expected to label the slice with `name` and to reveal
  // `arg` (if provided) when the slice is in focus.
  //
  // The first event of the trace should be logged with `log_first_begin_event`
  // instead of `log_begin_event`.
  void log_begin_event (std::string_view name, std::string_view arg);
  void log_begin_event (std::string_view name);
  void log_first_begin_event (std::string_view name);

  // Log the end of a Duration Event (type "E").  Duration events must be
  // properly paired and nested, so when possible, call `log_scope` rather than
  // `log_begin_event` and `log_end_event` to help avoid mistakes.
  //
  // `arg` serves the same purpose as in `log_begin_event`.  It is accepted here
  // to support cases where the information is unknown until later.
  //
  // The final event of the trace should be logged with `log_final_end_event`
  // instead of `log_end_event`.
  void log_end_event (std::string_view arg);
  void log_end_event ();
  void log_final_end_event (std::string_view arg = {});

  // Log the beginning of a Duration Event now (as if by calling
  // `log_begin_event` with the given arguments), and log the end as the
  // returned object goes out of scope (as if by calling `log_end_event` with no
  // arguments).
  //
  // This should be preferred over separate calls to `log_begin_event` and
  // `log_end_event` wherever it achieves the desired result.
  template <typename... Args>
  Scoped_slice log_scope (Args &&...args);

  // Log an Instant Event (type "i").
  void log_instant_event (std::string_view name);
};

// see `Time_tracer::log_scope`
class [[nodiscard]] Time_tracer::Scoped_slice final
{
private:
  Time_tracer &tracer_;

public:
  template <typename... Args>
  explicit Scoped_slice (Time_tracer &tracer, Args &&...args)
    : tracer_ (tracer)
  {
    tracer.log_begin_event (std::forward<Args> (args)...);
  }

  ~Scoped_slice () { tracer_.log_end_event (); }

  Scoped_slice (const Scoped_slice &) = delete;
  Scoped_slice &operator= (const Scoped_slice &) = delete;
};

template <typename... Args>
inline Time_tracer::Scoped_slice
Time_tracer::log_scope (Args &&...args)
{
  return Scoped_slice (*this, std::forward<Args> (args)...);
}

#endif // TIME_TRACER_HH
