/*
  midioutput.hh -- declare Midi_output

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MIDIOUTPUT_HH
#define MIDIOUTPUT_HH
#include "pscore.hh"

struct Midi_output {
    Midi_stream*    midi_stream_l_;
    Midi_output(Score* score_l, Midi_def* );
    void do_staff(Staff*st_l, int count);

    Midi_def* midi_l_;
};
#endif // MIDIOUTPUT_HH
