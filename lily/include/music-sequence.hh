/*   
  music-sequence.hh -- declare Music_sequence
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  Music_sequence ();
  SCM music_list () const;
  void append_music (Music *);
  VIRTUAL_COPY_CONS (Music);
  
  Pitch do_relative_octave (Pitch p, bool b);
  static void transpose_list (SCM , Pitch);
  static void compress_list (SCM, Moment);
  
  virtual void transpose (Pitch );
  virtual void compress (Moment);

  static Moment cumulative_length (SCM) ;
  static Moment maximum_length (SCM) ;
  static Moment first_start (SCM list) ;
  static Moment minimum_start (SCM list);
  
protected:
  virtual Pitch to_relative_octave (Pitch);
};
#endif
