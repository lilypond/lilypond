/*
  my-lily-parser.hh -- declare My_lily_parser

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef MY_LILY_PARSER_HH
#define MY_LILY_PARSER_HH

#include "array.hh"
#include "duration.hh"
#include "input.hh"
#include "parray.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "smobs.hh"
#include "string.hh"

/**
   State for the parser.  Do not ever add any variables to parse
   musical content here.  We still have to remove default_duration_.

   TODO: interface is too complicated
*/
class My_lily_parser 
{
  DECLARE_SMOBS (My_lily_parser, );
  friend int yyparse (void*);

  Array<Input> define_spots_;

  char const* here_str0 () const;
  Simultaneous_music *get_chord (Pitch tonic,
				 Array<Pitch> *adds, Array<Pitch> *subs,
				 Pitch *inversion, Pitch* bass, Duration d);
  void set_chord_tremolo (int type_i);
  void set_last_duration (Duration const *);
  void set_last_pitch (Pitch const *);

public:
  My_lily_lexer *lexer_;
  Sources *sources_;
  Duration default_duration_;
  String output_basename_;
  Protected_scm header_;
  int score_count_;
  int book_count_;
  int fatal_error_;
  int error_level_;
  bool ignore_version_b_;
  SCM last_beam_start_;

  My_lily_parser (Sources *sources);

  DECLARE_SCHEME_CALLBACK (paper_description, ());
  
  Input here_input () const;
  Input pop_spot ();
  void beam_check (SCM); 
  void do_init_file ();
  void do_yyparse ();
  void parse_file (String init, String name, String out_name);
  void parse_string (String ly_code);
  void parser_error (String);
  void push_spot ();
  void set_yydebug (bool);
};

DECLARE_UNSMOB (My_lily_parser, my_lily_parser);

SCM ly_parse_file (SCM);
SCM ly_parse_string (SCM);
SCM ly_parser_add_book_and_score (SCM, SCM);

#endif /* MY_LILY_PARSER_HH */
