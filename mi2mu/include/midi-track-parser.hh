/*
  midi-track-parser.hh -- declare 

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef MIDI_TRACK_PARSER_HH
#define MIDI_TRACK_PARSER_HH

#include "proto.hh"
#include "cons.hh"
#include "moment.hh"
#include "mi2mu-proto.hh"
#include "midi-parser.hh"

class Midi_track_parser : public Midi_parser
{
public:

  Midi_track_parser (Midi_parser_info* info_l, int i);
  ~Midi_track_parser ();

  Moment at_mom ();
  Mudela_staff* parse (Mudela_column* col_l);

private:
  bool eot ();
  void note_end (Mudela_column* col_l, int channel_i, int pitch_i, int aftertouch_i );
  void note_end_all (Mudela_column* col_l) ;
  void parse_delta_time ();
  Mudela_item* parse_event (Mudela_column* col_l);
  void parse_header ();

  Moment at_mom_;
  Byte running_byte_;
  Cons_list<Mudela_note> open_note_l_list_;
  Mudela_staff* mudela_staff_p_;
  Midi_parser_info* track_info_p_;
};

#endif // MIDI_TRACK_PARSER_HH
