/*   
  simple-file-storage.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SIMPLE_FILE_STORAGE_HH
#define SIMPLE_FILE_STORAGE_HH

#include "file-storage.hh"

/**
  read file char by char and copy into a malloc array.
 */
class Simple_file_storage  : public File_storage
{
  char * data_;
  int len_;

  void load_stdin ();
  void load_file (String);
public:
  virtual char const*to_str0 () const;
  virtual int length () const;
  virtual ~Simple_file_storage ();
  Simple_file_storage (String);
};

#endif /* SIMPLE_FILE_STORAGE_HH */

