/*
  performance.hh -- declare Performance

  (c) 1997--2005 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PERFORMANCE_HH
#define PERFORMANCE_HH

#include "parray.hh"
#include "cons.hh"
#include "music-output.hh"

/* MIDI output.  */
class Performance : public Music_output
{
public:
  Performance ();
  ~Performance ();

  void add_element (Audio_element *p);

  void output (Midi_stream &midi_stream_r);
  void output_header_track (Midi_stream &midi_stream_r);

  void print () const;
  SCM process (String);

  Link_array<Audio_staff> audio_staffs_;
  Cons<Audio_element> *audio_elem_p_list_;
  Output_def *midi_;
};

#endif /* PERFORMANCE_HH */
