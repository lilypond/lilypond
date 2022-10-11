/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "includable-lexer.hh"

#include "file-name.hh"
#include "file-path.hh"
#include "international.hh"
#include "source-file.hh"
#include "sources.hh"
#include "warn.hh"

#include <sstream>

using std::string;

#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

#ifndef YY_START
#define YY_START ((yy_start - 1) / 2)
#define YYSTATE YY_START
#endif

extern bool relative_includes;

/** Set the new input file to NAME, remember old file.  */
void
Includable_lexer::new_input (const string &name, Sources *sources)
{
  string current_dir = dir_name (main_input_name_);
  if (relative_includes)
    current_dir = include_stack_.size ()
                    ? dir_name (include_stack_.back ()->name_string ())
                    : "";

  Source_file *file = sources->get_file (name, current_dir);
  if (!file)
    {
      string msg = _f ("cannot find file: `%s'", name);
      msg += "\n";
      msg += _f ("(search path: `%s')",
                 (current_dir.length () ? (current_dir + PATHSEP) : "")
                   + sources->search_path ().c_str ());
      LexerError (msg.c_str ());
      return;
    }
  new_input (file->name_string (), file);
}

void
Includable_lexer::new_input (const string &name, string data, Sources *sources)
{
  Source_file *file = new Source_file (name, data);
  sources->add (file);
  new_input (name, file);
}

void
Includable_lexer::new_input (const string &name, Source_file *file)
{
  debug_output ("[" + name, false);
  file_name_strings_.push_back (name);

  char_count_stack_.push_back (0);
  include_stack_.push_back (file);

  yypush_buffer_state (yy_create_buffer (file->get_istream (), YY_BUF_SIZE));
}

/** pop the inputstack.  conceptually this is a destructor, but it
    does not destruct the Source_file that Includable_lexer::new_input
    creates.  */
void
Includable_lexer::close_input ()
{
  include_stack_.pop_back ();
  char_count_stack_.pop_back ();
  debug_output ("]", false);
  yypop_buffer_state ();
}

char const *
Includable_lexer::here_str0 () const
{
  if (include_stack_.empty ())
    return 0;
  return include_stack_.back ()->c_str () + char_count_stack_.back ();
}

Includable_lexer::~Includable_lexer ()
{
  while (!include_stack_.empty ())
    close_input ();
}

Source_file *
Includable_lexer::get_source_file () const
{
  if (include_stack_.empty ())
    return 0;
  return include_stack_.back ();
}

void
Includable_lexer::skip_chars (size_t count)
{
  for (size_t i = 0; i < count; ++i)
    yyinput ();
  char_count_stack_.back () += count;
}
