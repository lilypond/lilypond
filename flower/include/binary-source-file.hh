//
//  binary-source-file.hh -- declare Binary_source_file
//
//  copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef BINARY_SOURCE_FILE_HH
#define BINARY_SOURCE_FILE_HH

#include "source-file.hh"

class Binary_source_file : public Source_file
{
public:
  Binary_source_file (String& filename_string );
  virtual ~Binary_source_file ();

  U8 get_U8 (); 
  U16 get_U16 ();
  U32 get_U32 ();
  Byte get_Byte () {return get_U8 (); }
  int get_int () { return get_U32 (); }
  
  virtual String error_string (char const* pos_str0 ) const;
  virtual int get_line (char const* pos_str0 ) const;
};

#endif // BINARY_SOURCE_FILE_HH
