/*
   path.cc - manipulation of paths and filenames.
*/

#include "config.h"
#include <stdio.h>
#include <errno.h>

#if HAVE_SYS_STAT_H 
#include <sys/stat.h>
#endif

#include "file-path.hh"


#ifndef PATHSEP
#define PATHSEP ':'
#endif

/* We don't have multiple roots, set this to '\0'? */
#ifndef ROOTSEP
#define ROOTSEP ':'
#endif

#ifndef DIRSEP
#define DIRSEP '/'
#endif

#ifndef EXTSEP
#define EXTSEP '.'
#endif

/* Join components to full path. */
String
Path::str () const
{
  String s;
  if (!root.empty_b ())
    s = root + to_str (ROOTSEP);
  if (!dir.empty_b ())
    s += dir + to_str (DIRSEP);
  s += base;
  if (!ext.empty_b ())
    s += to_str (EXTSEP) + ext;
  return s;
}

/**
   @param path the original full filename
   @return 4 components of the path. They can be empty
*/
Path
split_path (String path)
{
  Path p;
  int i = path.index_i (ROOTSEP);
  if (i >= 0)
    {
      p.root = path.left_str (i);
      path = path.right_str (path.length_i () - i - 1);
    }

  i = path.index_last_i (DIRSEP);
  if (i >= 0)
    {
      p.dir = path.left_str (i);
      path = path.right_str (path.length_i () - i - 1);
    }

  i = path.index_last_i ('.');
  if (i >= 0)
    {
      p.base = path.left_str (i);
      p.ext = path.right_str (path.length_i () - i - 1);
    }
  else
    p.base = path;
  return p;
}

void
File_path::parse_path (String p)
{
  int l;
  
  while ((l = p.length_i ()) )
    {
      int i = p.index_i (PATHSEP);
      if (i <0) 
	i = l;
      add (p.left_str (i));
      p = p.right_str (l- i - 1);
    }
}




/** Find a file.
  It will search in the current dir, in the construction-arg, and
  in any other added path, in this order.

  @return
  The full path if found, or empty string if not found
  */
String
File_path::find (String nm) const
{
  if (!nm.length_i () || (nm == "-") )
    return nm;
  for (int i=0; i < size (); i++)
    {
      String path  = elem (i);
      String sep = to_str (DIRSEP);
      String right (path.right_str (1));
      if (path.length_i () && right != sep)
	path += to_str (DIRSEP);

      path += nm;


#if 0
      /*
	Check if directory. TODO: encapsulate for autoconf
       */
      struct stat sbuf;
      if (stat (path.ch_C (), &sbuf) == ENOENT)
	continue;
      
      if (! (sbuf.st_mode & __S_IFREG))
	continue;
#endif
#if !STAT_MACROS_BROKEN
      struct stat sbuf;
      if (stat (path.ch_C (), &sbuf) == ENOENT)
	continue;
      
      if (S_ISDIR (sbuf.st_mode))
	continue;
#endif

      FILE *f = fopen (path.ch_C (), "r"); // ugh!
      if (f)
	{
	  fclose (f);
	  return path;
	}
    }
  return "";
}

/**
   Add a directory, return false if failed
 */
bool
File_path::try_add (String s)
{
  if (s == "")
    s =  ".";
  FILE  * f = fopen (s.ch_C (), "r");
  if (!f)
    return false;
  fclose (f);
    
  push (s);
  return true;
}

void
File_path::add (String s)
{
  push (s);
}

String
File_path::str () const
{
  String s;
  for (int i=0; i< size (); i++)
    {
      s = s + elem (i);
      if (i < size () -1 )
	s += ":";
    }
  return s;
}
