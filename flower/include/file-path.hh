/*
  file-path.hh -- declare Path and File_path

  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef FILE_PATH_HH
#define FILE_PATH_HH

#include "string.hh"
#include "array.hh"


/**    
  search in directories for a file.

   Abstraction of PATH variable. An interface for searching input files.
   Search a number of dirs for a file.

   TODO: add a unix style PATH interface 
*/

class Path
{
public:
  String root;
  String dir;
  String base;
  String ext;

  String string () const;
};

class File_path : private Array<String>
{
public:
  String find (String nm) const;

  Array<String>::push;
  String string ()const;
  bool try_add (String str);
  void add (String);
  void parse_path (String);
};

Path split_path (String path);

#endif /* FILE_PATH */
