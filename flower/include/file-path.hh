/*
  file-path.hh -- declare File_path

  source file of the Flower Library

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef FILE_PATH_HH
#define FILE_PATH_HH

#include "std-vector.hh"
#include "std-string.hh"

/**
   search in directories for a file.

   Abstraction of PATH variable. An interface for searching input files.
   Search a number of dirs for a file.

   TODO: add a unix style PATH interface
*/

class File_path
{
  vector<string> dirs_;

public:
  vector<string> directories () const;
  string find (string name) const;
  string find (string name, char const *extensions[]);
  string to_string () const;
  bool try_append (string str);
  void append (string str);
  void parse_path (string);
  void prepend (string str);
};

bool is_file (string file_name);
bool is_dir (string file_name);

#endif /* FILE_PATH */
