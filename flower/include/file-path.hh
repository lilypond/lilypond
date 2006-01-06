/*
  file-path.hh -- declare File_path

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

class File_path
{
  Array<String> dirs_;
public:
  Array<String> directories () const;
  String find (String name) const;
  String find (String name, char const *extensions[]);
  String to_string () const;
  bool try_append (String str);
  void append (String str);
  void parse_path (String);
  void prepend (String str);
};

bool is_file (String file_name);
bool is_dir (String file_name);

#endif /* FILE_PATH */
