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
  Array<std::string> dirs_;
public:
  Array<std::string> directories () const;
  std::string find (std::string name) const;
  std::string find (std::string name, char const *extensions[]);
  std::string to_string () const;
  bool try_append (std::string str);
  void append (std::string str);
  void parse_path (std::string);
  void prepend (std::string str);
};

bool is_file (std::string file_name);
bool is_dir (std::string file_name);

#endif /* FILE_PATH */
