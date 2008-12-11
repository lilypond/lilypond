/*
  music.hh -- declare Music

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSIC_HH
#define MUSIC_HH

#include "smobs.hh"
#include "moment.hh"
#include "pitch.hh"
#include "prob.hh"

#define is_mus_type(x) internal_is_music_type (ly_symbol2scm (x))

class Music : public Prob
{
public:
  Music (SCM init);
  Music (Music const &m);
  VIRTUAL_COPY_CONSTRUCTOR (Music, Music);

  Input *origin () const;
  void set_spot (Input);

  bool internal_is_music_type (SCM) const;

  Stream_event *to_event () const;

  DECLARE_SCHEME_CALLBACK (relative_callback, (SCM, SCM));
  Pitch to_relative_octave (Pitch);
  Pitch generic_to_relative_octave (Pitch);
  Moment get_length () const;
  Moment start_mom () const;
  void print () const;

  /// Transpose, with the interval central C to #p#
  void transpose (Pitch p);

  /// Scale the music in time by #factor#.
  void compress (Moment factor);
  
  // Broadcast the event in a context's event-source.
  void send_to_context (Context *c);

  DECLARE_SCHEME_CALLBACK (duration_length_callback, (SCM));
 
protected:
  virtual SCM copy_mutable_properties () const;
  virtual void type_check_assignment (SCM, SCM) const;
  virtual void derived_mark () const;
protected:
  SCM length_callback_;
  SCM start_callback_;
  friend SCM ly_extended_make_music (SCM, SCM);
};

Music *unsmob_music (SCM);
Music *make_music_by_name (SCM sym);
SCM ly_music_deep_copy (SCM);
SCM ly_camel_case_2_lisp_identifier (SCM name_sym);

extern SCM ly_music_p_proc;

/* common transposition function for music and event */
void transpose_mutable (SCM alist, Pitch delta);

#endif /* MUSIC_HH */
