/*
  performance.hh -- declare Performance

  (c)  1997--1998 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef PERFORMANCE_HH
#define PERFORMANCE_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "music-output.hh"

/** all stuff which goes onto midi. notes, signs, symbols in a score
     #Performance# contains the items, the columns.
    */

class Performance : public Music_output {
public:
  Performance ();
  ~Performance () {}

  void add (Audio_column*);
  void add_staff (Audio_staff* l);
  void add (Audio_element*p);

    
  void output (Midi_stream& midi_stream_r);
  void output_header_track (Midi_stream& midi_stream_r);

  void print() const;
  void process();

  Pointer_list<Audio_column*> audio_column_p_list_;
  Link_list<Audio_staff*> audio_staff_l_list_;
  Pointer_list<Audio_element*> audio_elem_p_list_;
  Midi_def  * midi_l_;
};

#endif // PERFORMANCE_HH
