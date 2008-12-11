/*
  file-name.hh -- declare File_name

  source file of the Flower Library

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef FILE_NAME_HH
#define FILE_NAME_HH

#include "std-vector.hh"
#include "std-string.hh"

std::string dir_name (std::string const file_name);
std::string get_working_directory ();

class File_name
{
public:
  string root_;
  string dir_;
  string base_;
  string ext_;

  File_name (string="");

  bool is_absolute () const;
  string to_string () const;
  File_name canonicalized () const;
  string dir_part () const;
  string file_part () const;
};

#endif /* FILE_NAME */
