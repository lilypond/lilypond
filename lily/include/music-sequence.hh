

/*   
  music-sequence.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef MUSIC_SEQUENCE_HH
#define MUSIC_SEQUENCE_HH
#include "music.hh"
/**
  Music can be a list of other "Music" elements
 */
class Music_sequence : public Music
{
public:
  Music_sequence (SCM h);

  SCM music_list () const;
  void append_music (Music *);
  VIRTUAL_COPY_CONS(Music);

  Pitch do_relative_octave (Pitch p, bool b);
  virtual void transpose (Pitch );
  void truncate (int k);
  virtual void compress (Moment);
  int length_i () const;
  Moment cumulative_length () const;
  Moment maximum_length () const;
  
protected:
  virtual Pitch to_relative_octave (Pitch);


};
#endif
