/*   
  relative-octave-check.hh -- declare Relative_octave_octave-check
  
  source file of  GNU LilyPond
  
  (c) 2003--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef RELATIVE_OCTAVE_CHECK_HH
#define RELATIVE_OCTAVE_CHECK_HH

#include "music.hh"

class Relative_octave_check : public Music
{
public:
  Relative_octave_check (SCM);
  VIRTUAL_COPY_CONSTRUCTOR (Music, Relative_octave_check);

  virtual Pitch to_relative_octave (Pitch);
};

#endif /* RELATIVE_OCTAVE_CHECK_HH */


