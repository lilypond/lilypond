//
// my-midi-parser.hh -- declare My_midi_parser
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MY_MIDI_PARSER_HH
#define MY_MIDI_PARSER_HH

#include "mi2mu-proto.hh"
#include "proto.hh"
#include "plist.hh"
#include "string.hh"
#include "moment.hh"

#include "string.hh"
#include "moment.hh"

int yyparse();

/** 
  An interface to the YACC midi parser.
  (midi_parser)
 */
class My_midi_parser {
public:
    My_midi_parser (String filename_str,Sources *);
    ~My_midi_parser();

    void add_score (Mudela_score* mudela_score_p);
    void error (char const* sz_l);
    int parse();
    void forward (int i);
    Moment at_mom();
    void note_begin (int channel_i, int pitch_i, int dyn_i);
    void note_end (int channel_i, int pitch_i, int aftertouch_i);
    void note_end_all();

    void reset();
    void set_division_4 (int division_4_i);
    void set_key (int accidentals_i, int minor_i);
    void set_meter (int num_i, int den_i, int clocks_i, int count_32_i);
    void set_tempo (int useconds_per_4_i);

    int bar_i_;

    // ugh
    int track_i_;
    String filename_str_;
    String copyright_str_;
    String instrument_str_;
    String track_name_str_;

    // ugh
    Mudela_key* mudela_key_p_;
    Mudela_meter* mudela_meter_p_;
    Mudela_tempo* mudela_tempo_p_;

    Mudela_staff* mudela_staff_l_;
    Mudela_score* mudela_score_p_;
    Mudela_column* mudela_column_l_;

private:
    Link_list<Mudela_note*> open_mudela_note_l_list_;

    int division_1_i_;

    char const* defined_ch_C_;
    int fatal_error_i_;
    My_midi_lexer* midi_lexer_p_;
};

extern My_midi_parser* midi_parser_l_g;

#endif // MY_MIDI_PARSER_HH

