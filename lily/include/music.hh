/*
  music.hh -- declare Music

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef MUSIC_HH
#define MUSIC_HH

#include "virtual-methods.hh"
#include "minterval.hh"
#include "music-constructor.hh"

#define is_mus_type(x) internal_is_music_type (ly_symbol2scm (x))


class Music
{
public:
  Music (SCM init);
  Music (Music const &m);
  VIRTUAL_COPY_CONSTRUCTOR (Music, Music);

  Input *origin () const; 
  void set_spot (Input);

  SCM internal_get_property (SCM) const;
  void internal_set_property (SCM , SCM val);
  SCM get_property_alist (bool mut) const;
  bool internal_is_music_type (SCM) const;
  int duration_log () const;
  
  DECLARE_SCHEME_CALLBACK(relative_callback,(SCM,SCM));
  virtual Pitch to_relative_octave (Pitch);
  String name () const;
  Moment get_length () const;
  Moment start_mom () const;
  void print () const;

  /// Transpose, with the interval central C to #p#
  void transpose (Pitch p);
  
  /// Scale the music in time by #factor#.
  void compress (Moment factor);
  
protected:
  DECLARE_SMOBS (Music,);
  SCM immutable_property_alist_;
  SCM mutable_property_alist_;
protected:
  SCM length_callback_;
  SCM start_callback_;
  friend SCM ly_extended_make_music (SCM, SCM);
};

DECLARE_TYPE_P(Music);
DECLARE_UNSMOB(Music,music);

Music *make_music_by_name (SCM sym);
SCM ly_music_deep_copy (SCM);
SCM ly_music_scorify (SCM, SCM);

#endif /* MUSIC_HH */
