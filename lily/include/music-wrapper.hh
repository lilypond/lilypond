/*   
  music-wrapper.hh -- declare Music_wrapper
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  Music * element_p_;
public:
  Music_wrapper (Music*);
  Music * element_l () const;
  virtual void transpose (Musical_pitch);
  virtual void do_print () const;
  
  VIRTUAL_COPY_CONS(Music);
  Music_wrapper (Music_wrapper const&);
  virtual Moment length_mom () const;
  virtual ~Music_wrapper ();
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  
};




#endif /* MUSIC_WRAPPER_HH */

