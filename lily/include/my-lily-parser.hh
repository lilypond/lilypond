/*
  my-lily-parser.hh -- declare My_lily_parser

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MY_LILY_PARSER_HH
#define MY_LILY_PARSER_HH
#include "lily-proto.hh"
#include "duration.hh"
#include "string.hh"
#include "varray.hh"
#include "lily-proto.hh"
#include "lily-proto.hh"
#include "duration.hh"
#include "string.hh"
#include "varray.hh"
#include "input.hh"

class My_lily_parser {
  char const* here_ch_C() const;
  Array<Input> define_spot_array_;
  String init_str_;

  void add_requests (Chord*v);

  Chord * get_note_element (Note_req * ,Duration *);
  Chord* get_rest_element (String,Duration *);
  Chord* get_word_element (Text_def*, Duration*);
  Melodic_req* get_melodic_req (Melodic_req* melodic, int quotes);
  void set_nearest (Melodic_req* melodic);
  String notename_str (Melodic_req* melodic);

  void set_last_duration (Duration const *);
  void set_default_duration (Duration const *);
  void set_abbrev_beam (int type_i);
  void set_duration_mode (String s);
  void set_octave_mode (String s);
  friend int yyparse (void*);

public:
  int abbrev_beam_type_i_;
  int default_octave_i_;
  Melodic_req* last_melodic_;
  Duration default_duration_;
  Plet plet_;
  Header *default_header_p_;
    
  String textstyle_str_;
  bool first_b_;
  bool last_duration_mode_b_ ;
  bool relative_octave_mode_b_;
  Array<Request*> pre_reqs, post_reqs;
  int fatal_error_i_;
  Sources * source_l_;
  int error_level_i_;
  bool init_parse_b_;
  My_lily_lexer * lexer_p_;
 
  Moment plet_mom();
  void add_notename (String, Melodic_req* req_p);
  Input here_input() const;
  void remember_spot();
  Input pop_spot();
    
  Paper_def*default_paper();
  Midi_def*default_midi();
  void do_yyparse();
  void parser_error (String);
  void clear_notenames();

  Array<Request*>* get_parens_request (int t);
    
  void set_debug();
  void set_yydebug (bool);
  bool ignore_version_b_;
public:
  void do_init_file();
  void parse_file ( String init_str, String file_str);
  My_lily_parser (Sources * sources_l);
  ~My_lily_parser();
  void set_version_check (bool ignore);
};

#endif // MY_LILY_PARSER_HH
