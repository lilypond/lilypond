/*
  music-output-def.hh -- declare Music_output_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef Music_output_DEF_HH
#define Music_output_DEF_HH
#include "string.hh"
#include "lily-proto.hh"
#include "virtual-methods.hh"

/**
  Definition of how to output mudela. 
 */
class Music_output_def  
{
public:
  VIRTUAL_COPY_CONS(Music_output_def, Music_output_def);
  DECLARE_MY_RUNTIME_TYPEINFO;
  virtual void print () const {}
  virtual ~Music_output_def () {}
  virtual Global_translator * get_global_translator_p () { return 0; }
  String outfile_str_;
};
#endif // Music_output_DEF_HH
