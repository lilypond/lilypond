//
// lilypond-voice.hh -- declare Lilypond_voice
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef LILYPOND_VOICE_HH
#define LILYPOND_VOICE_HH

#include "midi2ly-proto.hh"
#include "cons.hh"

/// (lilypond_voice)
class Lilypond_voice
{
public:
  Lilypond_voice (Lilypond_staff* lilypond_staff_l);
  void add_item (Lilypond_item* lilypond_item_l);
  void output (Lilypond_stream& lilypond_stream_r);
  String get_clef () const;
  Lilypond_item * last_item_l_;
  Lilypond_note * last_note_l_;
private:
  Lilypond_staff* lilypond_staff_l_;
  Cons_list<Lilypond_item> lilypond_item_l_list_;

};

#endif // LILYPOND_VOICE_HH

