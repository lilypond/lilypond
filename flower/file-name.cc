/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include "file-name.hh"

#include <cstdio>
#include <cerrno>
#include <unistd.h>
#include <limits.h>

using namespace std;

#include "config.hh"

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif

#ifndef ROOTSEP
#define ROOTSEP ':'
#endif

#ifndef DIRSEP
#define DIRSEP '/'
#endif

#ifndef EXTSEP
#define EXTSEP '.'
#endif

#ifdef __CYGWIN__
static string
dos_to_posix (string file_name)
{
  char buf[PATH_MAX] = "";
  char s[PATH_MAX] = {0};
  file_name.copy (s, PATH_MAX - 1);
  /* ugh: char const* argument gets modified.  */
  int fail = cygwin_conv_to_posix_path (s, buf);
  if (!fail)
    return buf;
  return file_name;
}
#endif /* __CYGWIN__ */

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
dir_name (string const file_name)
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
  char cwd[PATH_MAX];
  getcwd (cwd, PATH_MAX);

  return string (cwd);
}

/* Join components to full file_name. */
string
File_name::dir_part () const
{
  string s;
  if (!root_.empty ())
    s = root_ + ::to_string (ROOTSEP);

  if (!dir_.empty ())
    {
      s += dir_;
    }

  return s;
}


string
File_name::file_part () const
{
  string s;
  s = base_;
  if (!ext_.empty ())
    s += ::to_string (EXTSEP) + ext_;
  return s;
}

string
File_name::to_string () const
{
  string d = dir_part ();
  string f = file_part ();

  if (!f.empty ()
      && !dir_.empty())
    {
      d += ::to_string (DIRSEP);
    }

  return d + f;
}

File_name::File_name (string file_name)
{
#ifdef __CYGWIN__
  /* All system functions would work, even if we do not convert to
     posix file_name, but we would think that \foe\bar\baz.ly is in
     the cwd.  */
  file_name = dos_to_posix (file_name);
#endif
#ifdef __MINGW32__
  file_name = slashify (file_name);
#endif

  ssize i = file_name.find (ROOTSEP);
  if (i != NPOS)
    {
      root_ = file_name.substr (0, i);
      file_name = file_name.substr (i + 1);
    }

  i = file_name.rfind (DIRSEP);
  if (i != NPOS)
    {
      dir_ = file_name.substr (0, i);
      file_name = file_name.substr (i + 1);
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
  /*
    Hmm. Is c:foo absolute?  
   */
  return (dir_.length () && dir_[0] == DIRSEP) || root_.length ();
}



File_name
File_name::canonicalized () const
{
  File_name c = *this;

  replace_all (&c.dir_, string ("//"), string ("/"));

  vector<string> components =  string_split (c.dir_, '/');
  vector<string> new_components;

  for (vsize i = 0; i < components.size (); i++)
    {
      if (components[i] == "..")
	new_components.pop_back ();
      else
	new_components.push_back (components[i]);
    }

  c.dir_ = string_join (new_components,  "/");
  return c;  
}
