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

#include "sources.hh"

#include "source-file.hh"
#include "file-name.hh"
#include "file-path.hh"

using std::string;
using std::vector;

Sources::Sources ()
{
  path_ = 0;
}

void
Sources::set_path (File_path *f)
{
  path_ = f;
}

/**
   Open a file. If the name is not absolute, look in CURRENT_DIR first.
   Afterwards, check the rest of the path_.

   FILE_STRING the name of the file to be opened.
   CURRENT_DIR a path to a directory, either absolute or relative to the
     working directory.
*/
Source_file *
Sources::get_file (string file_string, string const &current_dir)
{
  if (file_string != "-")
    {
      file_string = find_full_path (file_string, current_dir);
    }

  if (file_string.empty ())
    {
      return nullptr;
    }

  Source_file *f = new Source_file (file_string);
  add (f);
  return f;
}

string
Sources::find_full_path (string file_string, string const &current_dir) const
{
  // First, check for a path relative to the directory of the
  // file currently being parsed.
  if (current_dir.length () && file_string.length ()
      && !File_name (file_string).is_absolute ()
      && is_file (current_dir + DIRSEP + file_string))
    file_string = current_dir + DIRSEP + file_string;

  // Otherwise, check the rest of the path.
  else if (path_)
    {
      return path_->find (file_string);
    }
  return file_string;
}

string
Sources::search_path () const
{
  return path_->to_string ();
}

void
Sources::add (Source_file *sourcefile)
{
  sourcefiles_.push_back (sourcefile);
}

Sources::~Sources ()
{
  for (vsize i = 0; i < sourcefiles_.size (); i++)
    {
      sourcefiles_[i]->unprotect ();
    }
}

#include "lily-parser.hh"
#include "lily-lexer.hh"
#include "lily-imports.hh"
#include "fluid.hh"

LY_DEFINE (ly_source_files, "ly:source-files", 0, 1, 0, (SCM parser_smob),
           R"(
Return a list of input files that have been opened up to here, including the
files that have been closed already.  A parser, @var{parser-smob}, may
optionally be specified.
           )")
{
  if (SCM_UNBNDP (parser_smob))
    parser_smob = scm_fluid_ref (Lily::f_parser);
  auto *const parser = LY_ASSERT_SMOB (Lily_parser, parser_smob, 1);
  Includable_lexer *lex = parser->lexer_;

  SCM lst = SCM_EOL;
  for (vector<string>::const_iterator i = lex->file_name_strings_.begin ();
       i != lex->file_name_strings_.end (); ++i)
    {
      lst = scm_cons (ly_string2scm (*i), lst);
    }
  return scm_reverse_x (lst, SCM_EOL);
}

LY_DEFINE (ly_note_extra_source_file, "ly:note-extra-source-file", 1, 1, 0,
           (SCM filename, SCM parser),
           R"(
Register a file, e.g., an image file, as being needed to compile the
current file.  This is used for the @code{-dembed-@/source-@/code} option.
A parser may optionally be specified.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, filename, 1);
  if (SCM_UNBNDP (parser))
    parser = scm_fluid_ref (Lily::f_parser);
  auto *const p = LY_ASSERT_SMOB (Lily_parser, parser, 2);
  p->lexer_->file_name_strings_.push_back (ly_scm2string (filename));
  return SCM_UNSPECIFIED;
}
