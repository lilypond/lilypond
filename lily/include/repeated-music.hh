/*   
  repeated-music.hh -- declare Repeated_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef REPEATED_MUSIC_HH
#define REPEATED_MUSIC_HH

#include "music-list.hh"

/**
   Repeats and voltas
 */
class Repeated_music : public Music
{
public:
  int repeats_i_;
  bool unfold_b_;
  Music* repeat_p_;
  Music_sequence* alternative_p_;

  Repeated_music (Music*, int n, Music_sequence*);
  Repeated_music (Repeated_music const& s);
  virtual ~Repeated_music ();
  
  virtual void do_print () const;
  virtual void transpose (Musical_pitch p);
  virtual Moment length_mom () const;
  virtual Musical_pitch to_relative_octave (Musical_pitch p);
  VIRTUAL_COPY_CONS(Music);
};

#endif /* REPEATED_MUSIC_HH */

