/*
  file-name.cc - implement File_name

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
static std::string
dos_to_posix (std::string file_name)
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

/** Use slash as directory separator.  On Windows, they can pretty
    much be exchanged.  */
static std::string
slashify (std::string file_name)
{
  replace_all (file_name, '\\', '/');
  replace_all (file_name, std::String ("//"), "/");
  return file_name;
}

/* Join components to full file_name. */
std::string
File_name::to_string () const
{
  std::string s;
  if (!root_.empty ())
    s = root_ + std::to_string (ROOTSEP);
  if (!dir_.empty ())
    {
      s += dir_;
      if (!base_.empty () || !ext_.empty ())
	s += std::to_string (DIRSEP);
    }
  s += base_;
  if (!ext_.empty ())
    s += std::to_string (EXTSEP) + ext_;
  return s;
}

File_name::File_name (std::string file_name)
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

#if 0 //STD_STRING
File_name::File_name (String file_name)
{
  *this = File_name (std::string (file_name));
}
#endif
