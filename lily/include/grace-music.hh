/*   
  grace-music.hh -- declare Grace_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef GRACE_MUSIC_HH
#define GRACE_MUSIC_HH

#include "music-wrapper.hh"

class Grace_music : public Music_wrapper
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Music, Grace_music);
  Grace_music (SCM);
protected:
  virtual Moment start_mom () const;
};

#endif /* GRACE_MUSIC_HH */

