/*
  music-output.hh -- declare Music_output

  source file of the GNU LilyPond music typesetter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

  virtual void process (String) {} 
  virtual ~Music_output (){}
  Music_output () 
  {
  }
       
};
#endif // Music_output_HH
