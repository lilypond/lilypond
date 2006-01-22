/*
  file-path.cc - implement File_path

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "file-path.hh"

#include <cstdio>
#include <cerrno>
using namespace std;

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

Array<String>
File_path::directories () const
{
  return dirs_;
}

void
File_path::parse_path (String p)
{
  int len;
  while ((len = p.length ()))
    {
      int i = p.index (PATHSEP);
      if (i < 0)
	i = len;
      append (p.left_string (i));
      p = p.right_string (len - i - 1);
    }
}

bool
is_file (String file_name)
{
#if 0 /* Check if directory. TODO: encapsulate for autoconf */
  struct stat sbuf;
  if (stat (file_name.to_str0 (), &sbuf) != 0)
    return false;

  if (! (sbuf.st_mode & __S_IFREG))
    return false;
#endif

#if !STAT_MACROS_BROKEN
  struct stat sbuf;
  if (stat (file_name.to_str0 (), &sbuf) != 0)
    return false;

  return !S_ISDIR (sbuf.st_mode);
#endif

  if (FILE *f = fopen (file_name.to_str0 (), "r"))
    {
      fclose (f);
      return true;
    }

  return false;
}

bool
is_dir (String file_name)
{
#if !STAT_MACROS_BROKEN
  struct stat sbuf;
  if (stat (file_name.to_str0 (), &sbuf) != 0)
    return false;

  return S_ISDIR (sbuf.st_mode);
#endif

  if (FILE *f = fopen (file_name.to_str0 (), "r"))
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

String
File_path::find (String name) const
{
  if (!name.length () || (name == "-"))
    return name;

#ifdef __MINGW32__
  if (name.index ('\\') >= 0)
    programming_error ("file name not normalized: " + name);
#endif /* __MINGW32__ */

  /* Handle absolute file name.  */
  File_name file_name (name);
  if (file_name.dir_[0] == DIRSEP && is_file (file_name.to_string ()))
    return file_name.to_string ();

  for (int i = 0; i < dirs_.size (); i++)
    {
      File_name file_name (name);
      File_name dir = (Std_string) dirs_[i];
      file_name.root_ = dir.root_;
      dir.root_ = "";
      if (file_name.dir_.empty ())
	file_name.dir_ = dir.to_string ();
      else if (!dir.to_string ().empty ())
	file_name.dir_ = dir.to_string ()
	  + ::to_string (DIRSEP) + file_name.dir_;
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
String
File_path::find (String name, char const *extensions[])
{
  if (name.is_empty () || name == "-")
    return name;
  
  File_name file_name (name);
  String orig_ext = file_name.ext_;
  for (int i = 0; extensions[i]; i++)
    {
      file_name.ext_ = orig_ext;
      if (*extensions[i] && !file_name.ext_.empty ())
	file_name.ext_ += ".";
      file_name.ext_ += extensions[i];
      String found = find (file_name.to_string ());
      if (!found.is_empty ())
	return found;
    }
  
  return "";
}

/** Append a directory, return false if failed.  */
bool
File_path::try_append (String s)
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

String
File_path::to_string () const
{
  String s;
  for (int i = 0; i < dirs_.size (); i++)
    {
      s = s + dirs_[i];
      if (i < dirs_.size () - 1)
	s += ::to_string (PATHSEP);
    }
  return s;
}

void
File_path::append (String str)
{
  dirs_.push (str);
}

void
File_path::prepend (String str)
{
  dirs_.insert (str, 0);
}
