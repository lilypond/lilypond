/*   
  part-combine-music.hh -- declare Part_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef PART_COMBINE_MUSIC_HH
#define PART_COMBINE_MUSIC_HH

#include "music.hh"


class Part_combine_music : public Music
{
public:
  VIRTUAL_COPY_CONS (Music);
  Part_combine_music (String, Music*, Music*);

  Music * first_l () const;
  Music * second_l () const;
  
  virtual void transpose (Musical_pitch);

  virtual Moment length_mom () const;
  virtual Musical_pitch to_relative_octave (Musical_pitch);
  virtual void compress (Moment);

  String what_str_;
};

#endif /* PART_COMBINE_MUSIC_HH */

