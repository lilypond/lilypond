//
// mudela-voice.hh -- declare Mudela_voice
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef MUDELA_VOICE_HH
#define MUDELA_VOICE_HH

#include "midi2ly-proto.hh"
#include "cons.hh"

/// (mudela_voice)
class Mudela_voice
{
public:
  Mudela_voice (Mudela_staff* mudela_staff_l);
  void add_item (Mudela_item* mudela_item_l);
  void output (Mudela_stream& mudela_stream_r);

private:
  Mudela_staff* mudela_staff_l_;
  Cons_list<Mudela_item> mudela_item_l_list_;
};

#endif // MUDELA_VOICE_HH

