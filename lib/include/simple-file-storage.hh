/*   
  simple-file-storage.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
  
 */

#ifndef SIMPLE_FILE_STORAGE_HH
#define SIMPLE_FILE_STORAGE_HH

#include "file-storage.hh"

/**
  read file char by char and copy into a malloc array.
 */
class Simple_file_storage  : public File_storage
{
  char * data_p_;
  int len_i_;
protected:    
  virtual char const*ch_C () const;
  virtual int length_i () const;
  virtual ~Simple_file_storage ();
public:
  Simple_file_storage (String);
};

#endif /* SIMPLE_FILE_STORAGE_HH */

