/*   
  lyric-combine-music.hh -- declare Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef LYRIC_COMBINE_MUSIC_HH
#define LYRIC_COMBINE_MUSIC_HH
#include "music.hh"


class Lyric_combine_music : public Music
{
public:
  Music * get_music () const;
  Music * get_lyrics () const;
  
  Lyric_combine_music ();

  VIRTUAL_COPY_CONS (Music);
  virtual Moment get_length () const;
};
#endif /* LYRIC_COMBINE_MUSIC_HH */

