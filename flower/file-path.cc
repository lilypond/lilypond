/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "file-path.hh"

#include <cerrno>
#include <cstdio>

#include "config.hh"
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif

#include "file-name.hh"
#include "warn.hh"

#ifndef PATHSEP
#define PATHSEP ':'
#endif

#include <algorithm>

using std::string;
using std::vector;

vector<string>
File_path::directories () const
{
  return dirs_;
}

void
File_path::parse_path (const string &p)
{
  concat (dirs_, string_split (p, PATHSEP));
}

bool
is_file (const string &file_name)
{
#if !STAT_MACROS_BROKEN
  struct stat sbuf;
  if (stat (file_name.c_str (), &sbuf) != 0)
    return false;

  return !S_ISDIR (sbuf.st_mode);
#endif

  if (FILE *f = fopen (file_name.c_str (), "r"))
    {
      fclose (f);
      return true;
    }

  return false;
}

bool
is_dir (string file_name)
{
  /*
    canonicalize; in particular, trailing slashes should disappear.
   */
  file_name = File_name (file_name).to_string ();

#if !STAT_MACROS_BROKEN
  struct stat sbuf;
  if (stat (file_name.c_str (), &sbuf) != 0)
    return false;

  return S_ISDIR (sbuf.st_mode);
#endif

  if (FILE *f = fopen (file_name.c_str (), "r"))
    {
      fclose (f);
      return true;
    }
  return false;
}

/** Find a file.

Check absolute file name, search in the current dir (DUH! FIXME!),
in the construction-arg (what's that?), and in any other appended
directory, in this order.

@return
The file name if found, or empty string if not found. */

string
File_path::find (const string &name) const
{
  if (!name.length () || (name == "-"))
    return name;

#ifdef __MINGW32__
  if (name.find ('\\') != NPOS)
    programming_error ("file name not normalized: " + name);
#endif /* __MINGW32__ */

  /* Handle absolute file name.  */
  File_name file_name (name);
  if (file_name.is_absolute ())
    {
      if (is_file (file_name.to_string ()))
        return file_name.to_string ();
      else
        return "";
    }

  for (vsize i = 0; i < dirs_.size (); i++)
    {
      File_name file_name (name);
      File_name dir = (string)dirs_[i];

      // update `file_name' to hold `dir' and `file_name' concatenated
      file_name.root_ = dir.root_;
      dir.root_ = "";

      file_name.is_absolute_ = dir.is_absolute_;
      dir.is_absolute_ = false;

      if (file_name.dir_.empty ())
        file_name.dir_ = dir.to_string ();
      else if (!dir.to_string ().empty ())
        file_name.dir_ = dir.to_string () + DIRSEP + file_name.dir_;

      if (is_file (file_name.to_string ()))
        return file_name.to_string ();
    }

  return "";
}

/*
  Try to find

  file.EXT,

  where EXT is from EXTENSIONS.
*/
string
File_path::find (const string &name, char const *extensions[])
{
  if (name.empty () || name == "-")
    return name;

  File_name file_name (name);
  string orig_ext = file_name.ext_;
  for (int i = 0; extensions[i]; i++)
    {
      file_name.ext_ = orig_ext;
      if (*extensions[i] && !file_name.ext_.empty ())
        file_name.ext_ += ".";
      file_name.ext_ += extensions[i];
      string found = find (file_name.to_string ());
      if (!found.empty ())
        return found;
    }

  return "";
}

/** Append a directory, return false if failed.  */
bool
File_path::try_append (string s)
{
  if (s == "")
    s = ".";
  if (is_dir (s))
    {
      append (s);
      return true;
    }
  return false;
}

string
File_path::to_string () const
{
  string s;
  for (vsize i = 0; i < dirs_.size (); i++)
    {
      s = s + dirs_[i];
      if (i < dirs_.size () - 1)
        s += PATHSEP;
    }
  return s;
}

void
File_path::append (const string &str)
{
  dirs_.push_back (str);
}

void
File_path::prepend (const string &str)
{
  dirs_.insert (dirs_.begin (), str);
}
