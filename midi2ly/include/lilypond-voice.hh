//
// lilypond-voice.hh -- declare Lilypond_voice
//
// (c) 1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef LILYPOND_VOICE_HH
#define LILYPOND_VOICE_HH

#include "midi2ly-proto.hh"
//#include "flower-proto.hh"
#include "parray.hh"
#include "cons.hh"

/// (lilypond_voice)
class Lilypond_voice
{
public:
  Lilypond_voice (Lilypond_staff* lilypond_staff_l);
  void add_items (Link_array<Lilypond_item>& items);
  void output (Lilypond_stream& lilypond_stream_r);
  String get_clef () const;

private:
  Lilypond_staff* lilypond_staff_l_;
  Link_array < Cons_list<Lilypond_item> > threads_;
  Rational mom_;
};

#endif // LILYPOND_VOICE_HH

