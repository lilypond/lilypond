/*
  file-path.cc - implement File_path
   
  source file of the Flower Library
  
  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "file-path.hh"

#include <cstdio>
#include <cerrno>

#include "config.hh"
#if HAVE_SYS_STAT_H 
#include <sys/stat.h>
#endif

#ifdef __CYGWIN__
#include <sys/cygwin.h>
#endif

#include "file-name.hh"

#ifndef PATHSEP
#define PATHSEP ':'
#endif

void
File_path::parse_path (String p)
{
  int len;
  while ((len = p.length ()) )
    {
      int i = p.index (PATHSEP);
      if (i <0) 
	i = len;
      append (p.left_string (i));
      p = p.right_string (len - i - 1);
    }
}

/** Find a file.
    
  Seach in the current dir (DUH! FIXME?), in the construction-arg
  (what's that?, and in any other appended directory, in this order.

  @return
  The file name if found, or empty string if not found. */

String
File_path::find (String name) const
{
  if (!name.length () || (name == "-") )
    return name;

  /*
    TODO:  should check for absolute path
   */
  if (FILE *f =fopen (name.to_str0 (), "r"))
    {
      fclose (f);
      return name;
    }
       
  for (int i = 0; i < size (); i++)
    {
      String file_name = elem (i);
      String sep = ::to_string (DIRSEP);
      String right (file_name.right_string (1));
      if (file_name.length () && right != sep)
	file_name += ::to_string (DIRSEP);

      file_name += name;

#if 0 /* Check if directory. TODO: encapsulate for autoconf */
      struct stat sbuf;
      if (stat (file_name.to_str0 (), &sbuf) != 0)
	continue;
      
      if (! (sbuf.st_mode & __S_IFREG))
	continue;
#endif
#if !STAT_MACROS_BROKEN
      
      struct stat sbuf;
      if (stat (file_name.to_str0 (), &sbuf) != 0)
	continue;

      if (S_ISDIR (sbuf.st_mode))
	continue;
#endif

      /* ugh */
      FILE *f = fopen (file_name.to_str0 (), "r");
      if (f)
	{
	  fclose (f);
	  return file_name;
	}
    }
  return "";
}

/** Find a file.
    
  Seach in the current dir (DUH! FIXME?), in the construction-arg
  (what's that?, and in any other appended directory, in this order.

  Search for NAME, or name without extension, or name with any of
  EXTENSIONS, in that order.

  @return
  The file name if found, or empty string if not found. */
String
File_path::find (String name, char const *extensions[])
{
  File_name file_name (name);
  if (name.is_empty () || name == "-")
    file_name.base_ = "-";
  else
    {
      String orig_ext = file_name.ext_;
      for (int i = 0; extensions[i]; i++)
	{
	  file_name.ext_ = orig_ext;
	  if (*extensions[i] && !file_name.ext_.is_empty ())
	    file_name.ext_ += ".";
	  file_name.ext_ += extensions[i];
	  if (!find (file_name.to_string ()).is_empty ())
	    break;
	}
      /* Reshuffle extension */
      file_name = File_name (file_name.to_string ());
    }
  return file_name.to_string ();
}

/** Append a directory, return false if failed.  */
bool
File_path::try_append (String s)
{
  if (s == "")
    s =  ".";
  if (FILE *f = fopen (s.to_str0 (), "r"))
    {
      fclose (f);
      append (s);
      return true;
    }
  return false;
}

String
File_path::to_string () const
{
  String s;
  int n = size ();
  for (int i = 0; i < n; i++)
    {
      s = s + elem (i);
      if (i < n - 1)
	s += ":";
    }
  return s;
}
