/*
  midi-def.hh -- declare Midi_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef MIDI_DEF_HH
#define MIDI_DEF_HH

#include "lily-proto.hh"
#include "real.hh"
#include "string.hh"
#include "moment.hh"
#include "music-output-def.hh"

/** 
  definitions for midi output. Rather empty
 */
class Midi_def : public Music_output_def {
  static int default_count_i_;

public:
  VIRTUAL_COPY_CONS(Music_output_def);

  /// duration of whole note measured in seconds.
  Moment whole_in_seconds_mom_;

  Midi_def();
  ~Midi_def();

  Real length_mom_to_seconds_f (Moment);
  int get_tempo_i (Moment moment);
  void print() const;
  void set_tempo (Moment moment, int count_per_minute_i);
  virtual int get_next_default_count () const;
  static void reset_default_count();
};

#endif // MIDI_DEF_HH
