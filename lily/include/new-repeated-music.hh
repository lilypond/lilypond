/*   
  new-repeated-music.hh -- declare New_repeated_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef NEW_REPEATED_MUSIC_HH
#define NEW_REPEATED_MUSIC_HH

#include "music.hh"

class New_repeated_music : public Music
{
public:
  bool unfold_b_;
  int repeats_i_;

  Music * repeat_begin_p_;
  Music_sequence * alternatives_p_;

  virtual Musical_pitch to_relative_octave (Musical_pitch);

  /// The duration of this piece of music
  virtual Moment length_mom () const;


  void print() const;
  /// Transpose, with the interval central C to #p#
  virtual void transpose (Musical_pitch p);

  /// Scale the music in time by #factor#.
  virtual void compress (Moment factor);
  VIRTUAL_COPY_CONS(Music);

  New_repeated_music ();
  New_repeated_music (New_repeated_music const&);
  ~New_repeated_music ();
protected:
  virtual void do_print() const;

};


#endif /* NEW_REPEATED_MUSIC_HH */
