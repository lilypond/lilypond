/*
  performance.hh -- declare Performance

  (c) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PERFORMANCE_HH
#define PERFORMANCE_HH

#include "std-vector.hh"
#include "music-output.hh"

/* MIDI output.  */
class Performance : public Music_output
{
public:
  Performance ();
  ~Performance ();
  DECLARE_CLASSNAME(Performance);

  void add_element (Audio_element *p);
  virtual void process ();
  void remap_grace_durations ();
  void output (Midi_stream &midi_stream) const;
  void output_header_track (Midi_stream &midi_stream) const;

  void print () const;
  void write_output (string filename) const;

  vector<Audio_staff*> audio_staffs_;
  vector<Audio_element*> audio_elements_;
  Output_def *midi_;
};

#endif /* PERFORMANCE_HH */
