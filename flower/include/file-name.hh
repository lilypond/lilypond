/*
  file-name.hh -- declare File_name

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

  bool is_absolute () const;
  String to_string () const;
};

#endif /* FILE_NAME */
