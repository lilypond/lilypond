/*   
  lyric-combine-music.hh -- declare Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  virtual void transpose (Pitch);

  VIRTUAL_COPY_CONS (Music);
  virtual Moment get_length () const;
  virtual Pitch to_relative_octave (Pitch);
  virtual void compress (Moment);
};
#endif /* LYRIC_COMBINE_MUSIC_HH */

