/*
  music-output.hh -- declare Music_output

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Music_output_HH
#define Music_output_HH

#include "string.hh"
#include "lily-proto.hh"

/**
  Output something that was defined in a mudela file. 
 */
class Music_output 
{
public:
  Scope * header_l_;
  String default_out_str_;
  String origin_str_;
  int errorlevel_i_;

  virtual void process() {} 
  virtual ~Music_output (){}
  Music_output() 
  {
    errorlevel_i_ = 0;
  }
       
};
#endif // Music_output_HH
