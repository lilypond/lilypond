//
// lilypond-staff.hh -- declare lilypond_staff
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef LILYPOND_STAFF_HH
#define LILYPOND_STAFF_HH

#include "midi2ly-proto.hh"
#include "flower-proto.hh"
#include "cons.hh"
#include "string.hh"

/// (lilypond_staff)
class Lilypond_staff
{
public:
  Lilypond_staff (int number_i, String copyright_str, String track_name_str, String instrument_str);

  void add_item (Lilypond_item* lilypond_item_p);
  void eat_voice (Cons_list<Lilypond_item>& items);
  String id_str ();
  String name_str ();
  void output (Lilypond_stream& lilypond_stream_r);
  void process ();

  String copyright_str_;
  String instrument_str_;
  String name_str_;
  Lilypond_key* lilypond_key_l_;
  Lilypond_time_signature* lilypond_time_signature_l_;
  Lilypond_tempo* lilypond_tempo_l_;
  int number_i_;

private:
  void output_lilypond_begin_bar (Lilypond_stream& lilypond_stream_r, Rational now_mom, int bar_i);

  Cons_list<Lilypond_voice> lilypond_voice_p_list_;
  Cons_list<Lilypond_item> lilypond_item_p_list_;
};

#endif // LILYPOND_STAFF_HH

