/*
  midi-def.hh -- declare Midi_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <jan@digicash.com>
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
  VIRTUAL_COPY_CONS(Midi_def, Music_output_def);
  DECLARE_MY_RUNTIME_TYPEINFO;

  /// duration of whole note measured in seconds.
  Moment whole_in_seconds_mom_;

  Midi_def();
  ~Midi_def();

  Real duration_to_seconds_f (Moment);
  int get_tempo_i (Moment moment);
  void print() const;
  void set_tempo (Moment moment, int count_per_minute_i);
  virtual int get_next_default_count () const;
};

#endif // MIDI_DEF_HH

