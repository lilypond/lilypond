/*   
  re-rhythmed-music.hh -- declare Re_rhythmed_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef RE_RHYTHMED_MUSIC_HH
#define RE_RHYTHMED_MUSIC_HH

#include "music-wrapper.hh"

class Re_rhythmed_music : public Music_wrapper
{
public:
  void do_print () const;
  Re_rhythmed_music (Music*, Music*);
  
  VIRTUAL_COPY_CONS(Music);
  virtual Music_iterator* to_rhythm (Music_iterator*);
};


#endif /* RE_RHYTHMED_MUSIC_HH */

