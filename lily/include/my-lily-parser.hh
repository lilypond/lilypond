/*
  my-lily-parser.hh -- declare My_lily_parser

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MY_LILY_PARSER_HH
#define MY_LILY_PARSER_HH
#include "lily-proto.hh"
#include "string.hh"
#include "array.hh"
#include "lily-proto.hh"
#include "lily-proto.hh"
#include "duration.hh"
#include "string.hh"
#include "array.hh"
#include "input.hh"

class My_lily_parser 
{
public:
  My_lily_parser (Sources * sources_l);
  ~My_lily_parser();

  void do_init_file();
  void parse_file ( String init_str, String file_str);
  void set_version_check (bool ignore);

public:
  Duration default_duration_;
  Extender_req* extender_req;
  Scope *default_header_p_;
    
  bool first_b_;

  Array<Request*> pre_reqs, post_reqs;
  int fatal_error_i_;
  Sources * source_l_;
  int error_level_i_;
  bool init_parse_b_;
  My_lily_lexer * lexer_p_;
 
  Input here_input() const;
  void remember_spot();
  Input pop_spot();
    
  Paper_def*default_paper_p();
  Midi_def*default_midi_p();
  void do_yyparse();
  void parser_error (String);

  Array<Request*>* get_parens_request (int t);
    
  void set_debug();
  void set_yydebug (bool);
  bool ignore_version_b_;

private:
  char const* here_ch_C() const;
  Array<Input> define_spot_array_;
  String init_str_;

  void add_requests (Simultaneous_music*v);

  Simultaneous_music * get_note_element (Note_req * ,Duration *);
  Simultaneous_music * get_chord (Musical_pitch, Array<Musical_pitch>*, Array<Musical_pitch>*, Musical_pitch*, Duration);
  Simultaneous_music* get_rest_element (String, Duration *);
  Simultaneous_music* get_word_element (String, Duration*);
  Melodic_req* get_melodic_req (Melodic_req* melodic, int quotes);
  String notename_str (Melodic_req* melodic);
  void set_last_duration (Duration const *);
  friend int yyparse (void*);
};

#endif // MY_LILY_PARSER_HH
