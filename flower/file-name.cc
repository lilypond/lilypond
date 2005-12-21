/*
  file-name.cc - implement File_name

  source file of the Flower Library

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "file-name.hh"

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
static String
dos_to_posix (String file_name)
{
  char buf[PATH_MAX] = "";
  char *s = file_name.get_copy_str0 ();
  /* ugh: char const* argument gets modified.  */
  int fail = cygwin_conv_to_posix_path (s, buf);
  delete s;
  if (!fail)
    return buf;
  return file_name;
}
#endif /* __CYGWIN__ */

#ifdef __MINGW32__
/** Use slash as directory separator.  On Windows, they can pretty
    much be exchanged.  */
static String
slashify (String file_name)
{
  file_name.substitute ('\\', '/');
  file_name.substitute ("//", "/");
  return file_name;
}
#endif /* __MINGW32__ */

/* Join components to full file_name. */
String
File_name::to_string () const
{
  String s;
  if (!root_.is_empty ())
    s = root_ + ::to_string (ROOTSEP);
  if (!dir_.is_empty ())
    {
      s += dir_;
      if (!base_.is_empty () || !ext_.is_empty ())
	s += ::to_string (DIRSEP);
    }
  s += base_;
  if (!ext_.is_empty ())
    s += ::to_string (EXTSEP) + ext_;
  return s;
}

File_name::File_name (String file_name)
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

  int i = file_name.index (ROOTSEP);
  if (i >= 0)
    {
      root_ = file_name.left_string (i);
      file_name = file_name.right_string (file_name.length () - i - 1);
    }

  i = file_name.index_last (DIRSEP);
  if (i >= 0)
    {
      dir_ = file_name.left_string (i);
      file_name = file_name.right_string (file_name.length () - i - 1);
    }

  i = file_name.index_last ('.');
  if (i >= 0)
    {
      base_ = file_name.left_string (i);
      ext_ = file_name.right_string (file_name.length () - i - 1);
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
