/*
  file-path.hh -- declare File_name and File_path

  source file of the Flower Library

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef FILE_PATH_HH
#define FILE_PATH_HH

#include "array.hh"
#include "string.hh"

/**    
  search in directories for a file.

   Abstraction of PATH variable. An interface for searching input files.
   Search a number of dirs for a file.

   TODO: add a unix style PATH interface 
*/

class File_path : private Array<String>
{
public:
  String find (String name) const;
  String find (String name, char const *extensions[]);
  String to_string () const;
  bool try_append (String str);
  void append (String str) { Array<String>::push (str); }
  void parse_path (String);
  void prepend (String str) { Array<String>::insert (str, 0); }
};

#endif /* FILE_PATH */
