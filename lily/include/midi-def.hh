/*
  midi-def.hh -- declare Midi_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
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
public:
  // ugh!
  static int den_i_s;
  static int num_i_s;
  VIRTUAL_COPY_CONS(Midi_def, Music_output_def);
  DECLARE_MY_RUNTIME_TYPEINFO;
  Input_translator* itrans_p_;

  /// duration of whole note
  Real whole_seconds_f_;

  Midi_def();
  Midi_def (Midi_def const& midi_c_r);
  ~Midi_def();

  Real duration_to_seconds_f (Moment);
  int get_tempo_i (Moment moment);
  void print() const;
  void set (Input_translator* itrans_p);
  void set_tempo (Moment moment, int count_per_minute_i);
protected:
  virtual Global_translator * get_global_translator_p ();  
};

#endif // MIDI_DEF_HH

