/*
  music-output.hh -- declare Music_output

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef Music_output_HH
#define Music_output_HH

#include "string.hh"
#include "lily-proto.hh"
#include "protected-scm.hh"

/**
  Output something that was defined in a lilypond file. 
 */
class Music_output 
{
public:
  Protected_scm header_;
  String default_out_string_;
  String origin_string_;
  int errorlevel_;

  virtual void process () {} 
  virtual ~Music_output (){}
  Music_output () 
  {
    errorlevel_ = 0;
  }
       
};
#endif // Music_output_HH
