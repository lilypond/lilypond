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
  Binary_source_file (String& filename_str );
  virtual ~Binary_source_file ();

  U8 get_U8 () { return *(U8*)forward_ch_C (1); }
  U16 get_U16 () { return *(U16*)forward_ch_C (2); }
  U32 get_U32 () { return *(U32*)forward_ch_C (4); }
  Byte get_Byte () {return get_U8 (); }
  int get_int () { return get_U32 (); }
  
  virtual String error_str (char const* pos_ch_C ) const;
  virtual int line_i (char const* pos_ch_C ) const;
};

#endif // BINARY_SOURCE_FILE_HH
