/*
  midi-score-parser.hh -- declare 

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef MIDI_SCORE_PARSER_HH
#define MIDI_SCORE_PARSER_HH

#include "midi-parser.hh"
#include "proto.hh"
#include "mi2mu-proto.hh"
#include "parray.hh"

class Midi_score_parser : public Midi_parser
{
public:
  Mudela_score* parse (String filename_str, Sources*);

private:
  void open (String filename_str, Sources*);

  void parse_header ();
  // why the #*&$#{%) has Link_array another behaviour than Link_list??
  //  int find_earliest_i (Link_array<Midi_track_parser*>& tracks);
  int find_earliest_i (Link_array<Midi_track_parser>& tracks);
  Mudela_score* parse_score ();
};	    

#endif // MIDI_SCORE_PARSER_HH
