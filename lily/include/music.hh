/*
  music.hh -- declare Music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/



#ifndef MUSIC_HH
#define MUSIC_HH

#include "virtual-methods.hh"
#include "minterval.hh"
#include "lily-proto.hh"
#include "string.hh"
#include "smobs.hh"
#include "music-constructor.hh"


#define get_mus_property(x) internal_get_mus_property(ly_symbol2scm(x))
#define set_mus_property(x,y) internal_set_mus_property(ly_symbol2scm (x), y)
#define is_mus_type(x) internal_is_music_type(ly_symbol2scm (x))

/** Music is anything that has duration and supports both time compression and
  transposition.
  
  In Lily, everything that can be thought to have a length and a pitch
 (which has a duration which can be transposed) is considered "music",

  Music is hierarchical: 

  @see Music_sequence


  TODO: make a equalp function for general music. 
  */
class Music {
public:
  Input *origin () const; 
  void set_spot (Input);  

  SCM internal_get_mus_property (SCM) const;
  void internal_set_mus_property (SCM , SCM val);
  SCM get_property_alist (bool mut) const;
  bool internal_is_music_type (SCM) const;
  
  virtual Pitch to_relative_octave (Pitch);

  /// The duration of this piece of music
  virtual Moment length_mom () const;
  virtual Moment start_mom () const;
  void print () const;
  /// Transpose, with the interval central C to #p#
  virtual void transpose (Pitch p);

  /// Scale the music in time by #factor#.
  virtual void compress (Moment factor);
  VIRTUAL_COPY_CONS (Music);
  Music ();
  Music (Music const &m);
protected:
  DECLARE_SMOBS (Music,);
  SCM immutable_property_alist_;
  SCM mutable_property_alist_;
  friend SCM ly_extended_make_music(SCM,SCM);
};


DECLARE_UNSMOB(Music,music);

Music* make_music_by_name (SCM sym);


#endif // MUSIC_HH
