/*   
  mapped-file-storage.hh -- declare Mapped_file_storage
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MAPPED_FILE_STORAGE_HH
#define MAPPED_FILE_STORAGE_HH



#include "file-storage.hh"

/**
  Use mmap to "copy"  a file into memory
 */
class Mapped_file_storage:public File_storage
{
public:
  Mapped_file_storage (String);    
protected:
  virtual char const* ch_C () const;
  virtual int length_i () const;
  virtual ~Mapped_file_storage ();
private:
  void open (String name);
  void close ();

  void map ();
  void unmap ();
  int fildes_i_;
  off_t size_off_;
  caddr_t data_caddr_;
};

#endif /* MAPPED_FILE_STORAGE_HH */

