/*   
  music-wrapper.hh -- declare Music_wrapper
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#ifndef MUSIC_WRAPPER_HH
#define MUSIC_WRAPPER_HH

#include "music.hh"
#include "musical-pitch.hh"

class Music_wrapper : public Music
{
public:
  Music * element_p_;
  Music_wrapper (Music*);
  virtual void transpose (Musical_pitch);
  virtual void do_print () const;
  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS (Music_wrapper, Music);
  Music_wrapper (Music_wrapper const&);
  virtual MInterval time_int () const;
  virtual ~Music_wrapper ();
};



#endif /* MUSIC_WRAPPER_HH */

