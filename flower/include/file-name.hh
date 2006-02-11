/*
  file-name.hh -- declare File_name

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef FILE_NAME_HH
#define FILE_NAME_HH

#include "std-vector.hh"
#include "std-string.hh"

class File_name
{
public:
  string root_;
  string dir_;
  string base_;
  string ext_;

  File_name (string);

  bool is_absolute () const;
  string to_string () const;
};

#endif /* FILE_NAME */
