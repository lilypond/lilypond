/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include "config.hh"

#include "file-name.hh"

#include "flower-proto.hh"
#include "std-vector.hh"

#include <cerrno>
#include <cstdio>
#include <limits.h>

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

using std::string;
using std::vector;

#ifndef ROOTSEP
#define ROOTSEP ':'
#endif

#ifndef DIRSEP
#define DIRSEP '/'
#endif

#ifndef EXTSEP
#define EXTSEP '.'
#endif

/** Use slash as directory separator.  On Windows, they can pretty
    much be exchanged.  */
#if 0
static /* avoid warning */
#endif
string
slashify (string file_name)
{
  replace_all (&file_name, '\\', '/');
  replace_all (&file_name, string ("//"), "/");
  return file_name;
}

string
dir_name (const string &file_name)
{
  string s = file_name;
  s = slashify (s);
  ssize n = s.length ();
  if (n && s[n - 1] == '/')
    s[n - 1] = 0;
  if (s.rfind ('/') != NPOS)
    s = s.substr (0, s.rfind ('/'));
  else
    s = "";

  return s;
}

string
get_working_directory ()
{
#ifdef PATH_MAX
  vector<char> cwd (PATH_MAX);
#else
  vector<char> cwd (1024);
#endif
  while (getcwd (cwd.data (), cwd.size ()) == NULL)
    {
      if (errno != ERANGE)
        {
          // getcwd () fails.
          return "";
        }
      cwd.resize (cwd.size () * 2);
    }
  return string (cwd.data ());
}

/* Join components to full file_name. */
string
File_name::dir_part () const
{
  string s;

  if (!root_.empty ())
    s = root_ + ROOTSEP;

  // handle special case of `/'
  if (dir_.empty () && is_absolute_)
    s += DIRSEP;

  if (!dir_.empty ())
    s += dir_;

  return s;
}

string
File_name::file_part () const
{
  string s = base_;

  if (!ext_.empty ())
    s += EXTSEP + ext_;

  return s;
}

string
File_name::to_string () const
{
  string d = dir_part ();
  string f = file_part ();

  if (!f.empty () && !dir_.empty ())
    d += DIRSEP;

  return d + f;
}

File_name::File_name (string file_name)
{
#ifdef __MINGW32__
  file_name = slashify (file_name);
#endif

  ssize i = file_name.find (ROOTSEP);
  if (i != NPOS)
    {
      root_ = file_name.substr (0, i);
      file_name = file_name.substr (i + 1);
    }

  // note: `c:foo' is not absolute
  i = file_name.find (DIRSEP);
  is_absolute_ = (i == 0 ? true : false);

  i = file_name.rfind (DIRSEP);
  if (i != NPOS)
    {
      dir_ = file_name.substr (0, i);
      file_name = file_name.substr (i + 1);
    }

  // handle `.' and `..' specially
  if (file_name == string (".") || file_name == string (".."))
    {
      if (!dir_.empty ())
        dir_ += DIRSEP;
      dir_ += file_name;
      return;
    }

  i = file_name.rfind ('.');
  if (i != NPOS)
    {
      base_ = file_name.substr (0, i);
      ext_ = file_name.substr (i + 1);
    }
  else
    base_ = file_name;
}

bool
File_name::is_absolute () const
{
  return is_absolute_;
}

File_name
File_name::absolute (string const &cwd) const
{
  if (is_absolute_)
    return *this;

  File_name abs;
  File_name cwd_name (cwd + "/file.ext");

  abs.is_absolute_ = true;
  /*
    See
    https://devblogs.microsoft.com/oldnewthing/20101011-00/?p=12563
    for more background on cwd per drive.
   */
  abs.root_ = cwd_name.root_;
  abs.dir_ = cwd_name.dir_;
  if (!dir_.empty ())
    abs.dir_ += "/" + dir_;
  abs.base_ = base_;
  abs.ext_ = ext_;
  return abs;
}

File_name
File_name::canonicalized () const
{
  File_name c = *this;

  replace_all (&c.dir_, string ("//"), string ("/"));

  vector<string> components = string_split (c.dir_, '/');
  vector<string> new_components;

  for (vsize i = 0; i < components.size (); i++)
    {
      if (i && components[i] == string ("."))
        continue;
      else if (new_components.size () && components[i] == string (".."))
        {
          string s = new_components.back ();
          new_components.pop_back ();
          if (!new_components.size ())
            {
              if (s == string ("."))
                new_components.push_back ("..");
              else
                new_components.push_back (".");
            }
        }
      else
        new_components.push_back (components[i]);
    }

  c.dir_ = string_join (new_components, "/");
  return c;
}
