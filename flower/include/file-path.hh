/*
  path.hh -- declare File_path

  source file of the Flower Library

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef PATH_HH
#define PATH_HH
#include "string.hh"
#include "array.hh"


/**    
  search in directories for a file.

   Abstraction of PATH variable. An interface for searching input files.
   Search a number of dirs for a file.

   TODO: add a unix style PATH interface 
   Should use kpathsea?
   
*/

class File_path : private Array<String>
{
public:
  /// locate a file in the search path
  String find (String nm) const;

  /// add to end of path.
  Array<String>::push;
  String str ()const;
  void add (String str);
  void parse_path (String);
};

/** split a path into its components.

  @params path

  @return
  String &drive, String &dirs, String &filebase, String &extension
 */
void split_path (String path, String &drive, String &dirs, String &filebase, String &extension);

#endif
