//
// mudela-staff.hh -- declare mudela_staff
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef MUDELA_STAFF_HH
#define MUDELA_STAFF_HH

#include "midi2ly-proto.hh"
#include "proto.hh"
#include "cons.hh"
#include "string.hh"

/// (mudela_staff)
class Mudela_staff
{
public:
  Mudela_staff (int number_i, String copyright_str, String track_name_str, String instrument_str);

  void add_item (Mudela_item* mudela_item_p);
  void eat_voice (Cons_list<Mudela_item>& items);
  String id_str ();
  String name_str ();
  void output (Mudela_stream& mudela_stream_r);
  void process ();

  String copyright_str_;
  String instrument_str_;
  String name_str_;
  Mudela_key* mudela_key_l_;
  Mudela_time_signature* mudela_time_signature_l_;
  Mudela_tempo* mudela_tempo_l_;
  int number_i_;

private:
  void output_mudela_begin_bar (Mudela_stream& mudela_stream_r, Moment now_mom, int bar_i);

  Cons_list<Mudela_voice> mudela_voice_p_list_;
  Cons_list<Mudela_item> mudela_item_p_list_;
};

#endif // MUDELA_STAFF_HH

