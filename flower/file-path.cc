/*
   path.cc - manipulation of paths and filenames.
*/
#include <stdio.h>
#include "file-path.hh"
#include "flower-debug.hh"

#ifndef DIRSEP
#define DIRSEP '/'
#endif

#ifndef PATHSEP
#define PATHSEP ':'
#endif

/**
   @param path the original full filename
   @return 4 components of the path. They can be empty
*/
void
split_path (String path,
	    String &drive, String &dirs, String &filebase, String &extension)
{
  // peel off components, one by one.
  int di = path.index_i (':');
  if (di >= 0)
    {
      drive = path.left_str (di + 1);
      path = path.right_str (path.length_i () - di -1);
    }
  else
    drive = "";

  di = path.index_last_i (DIRSEP);
  if (di >=0)
    {
      dirs = path.left_str (di + 1);
      path = path.right_str (path.length_i ()-di -1);
    }
  else
    dirs = "";

  di = path.index_last_i ('.');
  if (di >= 0)
    {
      filebase = path.left_str (di);
      extension =path.right_str (path.length_i ()-di);
    }
  else
    {
      extension = "";
      filebase = path;
    }
}

void
File_path::parse_path (String p)
{
  int l;
  
  while ( (l = p.length_i ()) )
    {
      int i = p.index_i(PATHSEP);
      if (i <0) 
	i = l;
      add (p.left_str(i));
      p = p.right_str (l- i - 1);
    }
}




/** find a file.
  It will search in the current dir, in the construction-arg, and
  in any other added path, in this order.

  @return
  The full path if found, or empty string if not found
  */
String
File_path::find (String nm) const
{
  fdebug << "looking for" << nm << ": ";
  if (!nm.length_i() || (nm == "-") )
    return nm;
  for (int i=0; i < size(); i++)
    {
      String path  = elem(i);
      String sep = to_str (DIRSEP);
      String right(path.right_str (1));
      if (path.length_i () && right != sep)
	path += to_str (DIRSEP);

      path += nm;

      fdebug << path << "? ";
      FILE *f = fopen (path.ch_C(), "r"); // ugh!
      if (f)
	{
	  fdebug << "found\n";
	  fclose (f);
	  return path;
	}
    }
  fdebug << '\n';
  return "";
}

void
File_path::add (String s)
{
   push (s); 
}
