/*
  file-name.hh -- declare File_name

  source file of the Flower Library

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef FILE_NAME_HH
#define FILE_NAME_HH

#include "array.hh"
#include "string.hh"

class File_name
{
public:
  String root_;
  String dir_;
  String base_;
  String ext_;

  File_name (String);

  String to_string () const;
  char const *to_str0 () const;
};

#endif /* FILE_NAME */
