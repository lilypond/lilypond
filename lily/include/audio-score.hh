/*
  audio-score.hh -- declare Audio_score

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef AUDIO_SCORE_HH
#define AUDIO_SCORE_HH

#include "proto.hh"
#include "parray.hh"
#include "plist.hh"

/** all stuff which goes onto midi. notes, signs, symbols in a score
     #Audio_score# contains the items, the columns.
    
    */

class Audio_score {
public:
    Audio_score( Midi_def* );

    void add( Audio_column* );

//    void output(Tex_stream &ts);

    void play( Audio_item* i, Audio_column* c );
    void print() const;
    void process();

    Pointer_list<Audio_column*> audio_column_p_list_;
    Midi_def *midi_l_;
};

#endif // AUDIO_SCORE_HH
