/*
  performance.hh -- declare Performance

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef PERFORMANCE_HH
#define PERFORMANCE_HH

#include "parray.hh"
#include "lily-proto.hh"
#include "cons.hh"
#include "music-output.hh"

/** all stuff which goes onto midi. notes, signs, symbols in a score
     #Performance# contains the items, the columns.
    */

class Performance : public Music_output {
public:
  Performance ();
  ~Performance ();

  void add_element (Audio_element*p);
    
  void output (Midi_stream& midi_stream_r);
  void output_header_track (Midi_stream& midi_stream_r);

  void print() const;
  void process();


  Link_array<Audio_staff> audio_staff_l_arr_;
  Cons<Audio_element> *audio_elem_p_list_;
  Midi_def  * midi_l_;
};

#endif // PERFORMANCE_HH
