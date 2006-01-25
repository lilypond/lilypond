/*
  file-name.hh -- declare File_name

  source file of the Flower Library

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef FILE_NAME_HH
#define FILE_NAME_HH

#include "array.hh"
#include "std-string.hh"

class File_name
{
public:
  Std_string root_;
  Std_string dir_;
  Std_string base_;
  Std_string ext_;

  File_name (Std_string);
#if 0// STD_STRING
  File_name (String);
#endif

  bool is_absolute () const;
  Std_string to_string () const;
};

#endif /* FILE_NAME */
