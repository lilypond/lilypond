/*   
  lyric-combine-music.hh -- declare Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef LYRIC_COMBINE_MUSIC_HH
#define LYRIC_COMBINE_MUSIC_HH
#include "music.hh"


class Lyric_combine_music : public Music
{
public:
  Music * music_l () const;
  Music * lyrics_l () const;
  
  Lyric_combine_music (Music*, Music*);
  virtual void transpose (Musical_pitch);
  virtual void do_print () const;
  VIRTUAL_COPY_CONS(Music);
  virtual Moment length_mom () const;
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual void compress (Moment);
};
#endif /* LYRIC_COMBINE_MUSIC_HH */

