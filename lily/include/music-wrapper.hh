/*   
  music-wrapper.hh -- declare Music_wrapper
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MUSIC_WRAPPER_HH
#define MUSIC_WRAPPER_HH

#include "music.hh"
#include "musical-pitch.hh"

/** A Music that modifies an existing Music.  This data structure
  corresponds to a production that takes a single Music argument,
  
  Music: STUFF Music 

  */
class Music_wrapper : public Music
{
public:
  Music_wrapper (Music*);
  Music * element () const;
  virtual void transpose (Musical_pitch);
  virtual void do_print () const;
  
  VIRTUAL_COPY_CONS(Music);
  virtual Moment length_mom () const;
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual void compress (Moment);
};




#endif /* MUSIC_WRAPPER_HH */

