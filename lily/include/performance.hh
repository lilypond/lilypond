/*
  performance.hh -- declare Performance

  (c) 1997--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PERFORMANCE_HH
#define PERFORMANCE_HH

#include "std-vector.hh"
#include "cons.hh"
#include "music-output.hh"

/* MIDI output.  */
class Performance : public Music_output
{
public:
  Performance ();
  ~Performance ();
  DECLARE_CLASSNAME(Performance);

  void add_element (Audio_element *p);

  void output (Midi_stream &midi_stream_r);
  void output_header_track (Midi_stream &midi_stream_r);

  void print () const;
  void write_output (std::string filename);

  Link_array__Audio_staff_ audio_staffs_;
  Cons<Audio_element> *audio_element_list_;
  Output_def *midi_;
};

#endif /* PERFORMANCE_HH */
