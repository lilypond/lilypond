/*   
  grace-music.hh -- declare Grace_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef NGRACE_MUSIC_HH
#define NGRACE_MUSIC_HH

#include "music-wrapper.hh"

class Grace_music : public Music_wrapper
{
public:
  Music *clone () const { return Grace_music (*this); } 
  Grace_music ();
protected:
  virtual Moment get_length () const;
  virtual Moment start_mom () const;
};

#endif /* GRACE_MUSIC_HH */

