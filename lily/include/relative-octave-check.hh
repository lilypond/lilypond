/*   
  relative-octave-check.hh -- declare Relative_octave_octave-check
  
  source file of  GNU LilyPond
  
  (c) 2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef RELATIVE_OCTAVE_CHECK_HH
#define RELATIVE_OCTAVE_CHECK_HH

#include "music.hh"

class Relative_octave_check : public Music
{
public:
  VIRTUAL_COPY_CONS(Music);
  virtual Pitch to_relative_octave (Pitch);
};


#endif /* RELATIVE_MUSIC_HH */


