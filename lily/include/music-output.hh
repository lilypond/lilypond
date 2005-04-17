/*
  music-output.hh -- declare Music_output

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef MUSIC_OUTPUT_HH
#define MUSIC_OUTPUT_HH

#include "string.hh"
#include "lily-proto.hh"
#include "protected-scm.hh"

class Music_output
{
public:
  virtual SCM process (String); 
  virtual ~Music_output ();
};

#endif /* MUSIC_OUTPUT_HH */
