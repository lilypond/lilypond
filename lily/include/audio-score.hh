/*
  audio-score.hh -- declare Audio_score

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef AUDIO_SCORE_HH
#define AUDIO_SCORE_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "music-output.hh"

/** all stuff which goes onto midi. notes, signs, symbols in a score
     #Audio_score# contains the items, the columns.
    
    */

class Audio_score : public Music_output {
public:
  Audio_score ();
  ~Audio_score () {}

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

#endif // AUDIO_SCORE_HH
