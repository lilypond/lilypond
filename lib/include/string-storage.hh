/*   
  string-storage.hh -- declare String_storage
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef STRING_STORAGE_HH
#define STRING_STORAGE_HH

#include "string.hh"
#include "file-storage.hh"

/**
 Urg, let String act as file storage.
 */
class String_storage : public File_storage, protected String
{
public:
  String_storage (String s) : String (s) { }

protected:    
  virtual char const* ch_C () const { return String::ch_C (); }
  virtual int length_i () const { return String::length_i (); }
};

#endif /* STRING_STORAGE_HH */

