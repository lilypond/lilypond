/*   
  lyric-combine-music.hh -- declare Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef LYRIC_COMBINE_MUSIC_HH
#define LYRIC_COMBINE_MUSIC_HH
#include "music.hh"


class Lyric_combine_music : public Music
{
public:
  Music * music_l () const;
  Music * lyrics_l () const;
  
  Lyric_combine_music ();
  Lyric_combine_music (SCM);
  virtual void transpose (Pitch);

  VIRTUAL_COPY_CONS (Music);
  virtual Moment length_mom () const;
  virtual Pitch to_relative_octave (Pitch);
  virtual void compress (Moment);
};
#endif /* LYRIC_COMBINE_MUSIC_HH */

