/*
  file-path.hh -- declare File_path

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef FILE_PATH_HH
#define FILE_PATH_HH

#include "array.hh"
#include "std-string.hh"

/**
   search in directories for a file.

   Abstraction of PATH variable. An interface for searching input files.
   Search a number of dirs for a file.

   TODO: add a unix style PATH interface
*/

class File_path
{
  Array<Std_string> dirs_;
public:
  Array<Std_string> directories () const;
  Std_string find (Std_string name) const;
  Std_string find (Std_string name, char const *extensions[]);
  Std_string to_string () const;
  bool try_append (Std_string str);
  void append (Std_string str);
  void parse_path (Std_string);
  void prepend (Std_string str);
};

bool is_file (Std_string file_name);
bool is_dir (Std_string file_name);

#endif /* FILE_PATH */
