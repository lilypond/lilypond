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

#include "file-path.hh"

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif

#ifdef __MINGW32__
#include <windows.h>
#endif

#include "file-name.hh"
#include "flower-proto.hh"
#include "std-vector.hh"
#include "warn.hh"

#ifndef PATHSEP
#define PATHSEP ':'
#endif

#include <algorithm>
#include <cerrno>
#include <cstdio>
#include <memory>
#include <unistd.h>

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
  dirs_ = string_split (p, PATHSEP);
}

#ifdef __MINGW32__
#define STRUCT_STAT struct _stat
#define FUNCTION_STAT _stat
#else
#define STRUCT_STAT struct stat
#define FUNCTION_STAT stat
#endif

inline int
workaround_wrapper_stat (const char *f, STRUCT_STAT *s)
{
#if !defined(__MINGW32__) || defined(_UCRT)
  return FUNCTION_STAT (f, s);
#else
  // Workaround for MinGW UTF-8 stat issue:
  // `stat ()` in msvcrt.dll returns `-1` (i.e. error) with some UTF-8
  // filenames even if process code page is UTF-8 and the file exists.
  // So we get the process code page with `GetACP ()` and, if it is UTF-8,
  // convert the filename to wide strings and use `_wstat ()`.
  // If Universal CRT (UCRT, newer than msvcrt.dll) is used
  // (i.e. `_UCRT` is defined), no such workaround is needed.
  if (GetACP () == CP_UTF8)
    {
      int len = MultiByteToWideChar (CP_UTF8, 0, f, -1, nullptr, 0);
      if (len == 0)
        return -1;

      std::unique_ptr<WCHAR[]> pw (new WCHAR[len]);
      MultiByteToWideChar (CP_UTF8, 0, f, -1, pw.get (), len);

      return _wstat (pw.get (), s);
    }

  return FUNCTION_STAT (f, s);
#endif
}

bool
is_file (const string &file_name)
{
  STRUCT_STAT sbuf;
  if (workaround_wrapper_stat (file_name.c_str (), &sbuf) != 0)
    return false;

  return !S_ISDIR (sbuf.st_mode);
}

bool
is_dir (string file_name)
{
  /*
    canonicalize; in particular, trailing slashes should disappear.
   */
  file_name = File_name (file_name).to_string ();

  STRUCT_STAT sbuf;
  if (workaround_wrapper_stat (file_name.c_str (), &sbuf) != 0)
    return false;

  return S_ISDIR (sbuf.st_mode);
}

bool
rename_file (const char *oldname, const char *newname)
{
#if !defined(__MINGW32__)
  return rename (oldname, newname) == 0;
#else
  // If the current code page is UTF-8, convert the filename to wide strings
  // and use MoveFileExW().
  if (GetACP () == CP_UTF8)
    {
      int old_len = MultiByteToWideChar (CP_UTF8, 0, oldname, -1, nullptr, 0);
      int new_len = MultiByteToWideChar (CP_UTF8, 0, newname, -1, nullptr, 0);
      if (old_len == 0 || new_len == 0)
        return false;

      std::unique_ptr<WCHAR[]> old_wide (new WCHAR[old_len]);
      MultiByteToWideChar (CP_UTF8, 0, oldname, -1, old_wide.get (), old_len);
      std::unique_ptr<WCHAR[]> new_wide (new WCHAR[new_len]);
      MultiByteToWideChar (CP_UTF8, 0, newname, -1, new_wide.get (), new_len);

      // Note the return value: MoveFileExW() returns 0 in case of failure, so
      // the opposite of POSIX rename().
      if (MoveFileExW (old_wide.get (), new_wide.get (),
                       MOVEFILE_REPLACE_EXISTING)
          != 0)
        return true;

      // Fall back to copying the file contents to the destination.
      if (CopyFileW (old_wide.get (), new_wide.get (), FALSE) != 0)
        {
          // The copy succeeded, now delete the source file.
          return DeleteFileW (old_wide.get ()) != 0;
        }

      // All failed, give up.
      return false;
    }

  // Note the return value: MoveFileExA() returns 0 in case of failure, so the
  // opposite of POSIX rename().
  if (MoveFileExA (oldname, newname, MOVEFILE_REPLACE_EXISTING) != 0)
    return true;

  // Fall back to copying the file contents to the destination.
  if (CopyFileA (oldname, newname, FALSE) != 0)
    {
      // The copy succeeded, now delete the source file.
      return DeleteFileA (oldname) != 0;
    }

  // All failed, give up.
  return false;
#endif
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
      File_name dir (dirs_[i]);

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
