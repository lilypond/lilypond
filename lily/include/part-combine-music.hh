/*   
  part-combine-music.hh -- declare Part_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef PART_COMBINE_MUSIC_HH
#define PART_COMBINE_MUSIC_HH

#include "music.hh"


class Part_combine_music : public Music
{
public:
  VIRTUAL_COPY_CONS (Music);
  Part_combine_music (SCM l);

  Music * first_l () const;
  Music * second_l () const;
  
  virtual void transpose (Pitch);

  virtual Moment length_mom () const;
  virtual Pitch to_relative_octave (Pitch);
  virtual void compress (Moment);

  Part_combine_music ();
};

#endif /* PART_COMBINE_MUSIC_HH */

