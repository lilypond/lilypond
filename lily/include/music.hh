/*
  music.hh -- declare Music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/



#ifndef MUSIC_HH
#define MUSIC_HH

#include "virtual-methods.hh"
#include "minterval.hh"
#include "lily-proto.hh"
#include "string.hh"
#include "smobs.hh"

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
  
  SCM get_mus_property (const char*) const;
  SCM get_mus_property (SCM) const;
  void set_mus_property (const char * , SCM val);
  void set_immutable_mus_property (const char * , SCM val);
  void set_immutable_mus_property (SCM key, SCM val);  
  void set_mus_property (SCM , SCM val);  
  void set_mus_pointer (const char*, SCM val);
  SCM remove_mus_property (const char* nm);

  virtual Pitch to_relative_octave (Pitch);

  /// The duration of this piece of music
  virtual Moment length_mom () const;

  void print() const;
  /// Transpose, with the interval central C to #p#
  virtual void transpose (Pitch p);

  /// Scale the music in time by #factor#.
  virtual void compress (Moment factor);
  VIRTUAL_COPY_CONS(Music);
  Music (Music const &m);
  Music();
protected:

  DECLARE_SMOBS(Music,);
  SCM immutable_property_alist_;
  SCM mutable_property_alist_;
 
};


Music * unsmob_music (SCM);
#endif // MUSIC_HH



