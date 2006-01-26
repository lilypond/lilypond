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
  std::string root_;
  std::string dir_;
  std::string base_;
  std::string ext_;

  File_name (std::string);
#if 0// STD_STRING
  File_name (String);
#endif

  bool is_absolute () const;
  std::string to_string () const;
};

#endif /* FILE_NAME */
