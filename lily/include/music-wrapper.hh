/*   
  music-wrapper.hh -- declare Music_wrapper
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  VIRTUAL_COPY_CONSTRUCTOR (Music, Music_wrapper);
  DECLARE_SCHEME_CALLBACK(length_callback, (SCM));
  DECLARE_SCHEME_CALLBACK(start_callback, (SCM));
  
  Music *element () const;
};

#endif /* MUSIC_WRAPPER_HH */

