/*   
  music-wrapper.hh -- declare Music_wrapper
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MUSIC_WRAPPER_HH
#define MUSIC_WRAPPER_HH

#include "music.hh"
#include "pitch.hh"

/** A Music that modifies an existing Music.  This data structure
  corresponds to a production that takes a single Music argument,
  
  Music: STUFF Music 

  */
class Music_wrapper : public Music
{
public:
  Music_wrapper (SCM);
  Music_wrapper ();
  Music * element () const;
  virtual void transpose (Pitch);

  
  VIRTUAL_COPY_CONS (Music);
  virtual Moment length_mom () const;
  virtual Moment start_mom () const;
  virtual Pitch to_relative_octave (Pitch);
  virtual void compress (Moment);
};




#endif /* MUSIC_WRAPPER_HH */

