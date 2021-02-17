/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "input.hh"

#include "international.hh"
#include "lily-imports.hh"
#include "program-option.hh"
#include "source-file.hh"
#include "sources.hh"
#include "warn.hh"

#include <cstdio>

using std::string;

Input
Input::spot () const
{
  return *this;
}

void
Input::set_spot (Input const &i)
{
  *this = i;
}

void
Input::step_forward ()
{
  if (end_ == start_)
    end_++;
  start_++;
}

void
Input::set_location (Input const &i_start, Input const &i_end)
{
  source_file_ = i_start.source_file_;
  start_ = i_start.start_;
  end_ = i_end.end_;
}

/*
  Produce GNU-compliant error message.  Correcting lilypond source is
  such a breeze if you ('re edidor) know (s) the error column too

  Format:

  [file:line:column:][warning:]message
*/
string
Input::message_string (const string &msg) const
{
  if (source_file_)
    return msg + "\n" + source_file_->quote_input (start_);
  else
    return msg;
}

string
Input::message_location () const
{
  return (source_file_) ? location_string () : "";
}

void
Input::error (const string &s) const
{
  ::error (message_string (s), message_location ());
}

void
Input::programming_error (const string &s) const
{
  ::programming_error (message_string (s), message_location ());
}

void
Input::non_fatal_error (const string &s) const
{
  ::non_fatal_error (message_string (s), message_location ());
}

void
Input::warning (const string &s) const
{
  ::warning (message_string (s), message_location ());
}

void
Input::message (const string &s) const
{
  ::message (message_string (s), true, message_location ());
}

void
Input::debug_output (const string &s) const
{
  ::debug_output (message_string (s), true, message_location ());
}

string
Input::location_string () const
{
  if (source_file_)
    return source_file_->file_line_column_string (start_);
  return " (" + _ ("position unknown") + ")";
}

string
Input::line_number_string () const
{
  if (source_file_)
    return std::to_string (source_file_->get_line (start_));
  return "?";
}

string
Input::file_string () const
{
  if (source_file_)
    return source_file_->name_string ();
  return "";
}

ssize_t
Input::line_number () const
{
  if (source_file_)
    return source_file_->get_line (start_);
  return 0;
}

ssize_t
Input::column_number () const
{
  ssize_t line, chr, col, offset = 0;
  source_file_->get_counts (start_, &line, &chr, &col, &offset);

  return col;
}

ssize_t
Input::end_line_number () const
{
  if (source_file_)
    return source_file_->get_line (end_);
  return 0;
}

ssize_t
Input::end_column_number () const
{
  ssize_t line, chr, col, offset = 0;
  source_file_->get_counts (end_, &line, &chr, &col, &offset);

  return col;
}

void
Input::get_counts (ssize_t *line, ssize_t *chr,
                   ssize_t *col, ssize_t *offset) const
{
  source_file_->get_counts (start_, line, chr, col, offset);
}

void
Input::set (Source_file *sf, char const *start, char const *end)
{
  source_file_ = sf;
  start_ = start;
  end_ = end;
}

Source_file *
Input::get_source_file () const
{
  return source_file_;
}

static SCM
with_location_hook_0 (void *it)
{
  SCM *args = static_cast <SCM *> (it);
  return scm_call_0 (args[0]);
}

SCM
with_location (SCM loc, SCM proc)
{
  return scm_c_with_fluid (Lily::f_location,
                           unsmob<Input> (loc) ? loc : SCM_BOOL_F,
                           with_location_hook_0,
                           static_cast <void *> (&proc));
}

static SCM
with_location_hook_1 (void *it)
{
  SCM *args = static_cast <SCM *> (it);
  return scm_call_1 (args[0], args[1]);
}

SCM
with_location (SCM loc, SCM proc, SCM arg1)
{
  SCM args[] = { proc, arg1 };
  return scm_c_with_fluid (Lily::f_location,
                           unsmob<Input> (loc) ? loc : SCM_BOOL_F,
                           with_location_hook_1,
                           static_cast <void *> (&args));
}

static SCM
with_location_hook_2 (void *it)
{
  SCM *args = static_cast <SCM *> (it);
  return scm_call_2 (args[0], args[1], args[2]);
}

SCM
with_location (SCM loc, SCM proc, SCM arg1, SCM arg2)
{
  SCM args[] = { proc, arg1, arg2 };
  return scm_c_with_fluid (Lily::f_location,
                           unsmob<Input> (loc) ? loc : SCM_BOOL_F,
                           with_location_hook_2,
                           static_cast <void *> (&args));
}

static SCM
with_location_hook_3 (void *it)
{
  SCM *args = static_cast <SCM *> (it);
  return scm_call_3 (args[0], args[1], args[2], args[3]);
}

SCM
with_location (SCM loc, SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  SCM args[] = { proc, arg1, arg2, arg3 };
  return scm_c_with_fluid (Lily::f_location,
                           unsmob<Input> (loc) ? loc : SCM_BOOL_F,
                           with_location_hook_3,
                           static_cast <void *> (&args));
}

static SCM
with_location_hook_4 (void *it)
{
  SCM *args = static_cast <SCM *> (it);
  return scm_call_4 (args[0], args[1], args[2], args[3], args[4]);
}

SCM
with_location (SCM loc, SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  SCM args[] = { proc, arg1, arg2, arg3, arg4 };
  return scm_c_with_fluid (Lily::f_location,
                           unsmob<Input> (loc) ? loc : SCM_BOOL_F,
                           with_location_hook_4,
                           static_cast <void *> (&args));
}

static SCM
with_location_hook_n (void *it)
{
  SCM *args = static_cast <SCM *> (it);
  return scm_apply_0 (args[0], args[1]);
}

SCM
with_location (SCM loc, SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)
{
  SCM args[] = { proc, scm_list_5 (arg1, arg2, arg3, arg4, arg5) };
  return scm_c_with_fluid (Lily::f_location,
                           unsmob<Input> (loc) ? loc : SCM_BOOL_F,
                           with_location_hook_n,
                           static_cast <void *> (&args));
}
