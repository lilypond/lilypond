/*
  my-lily-parser.hh -- declare My_lily_parser

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MY_LILY_PARSER_HH
#define MY_LILY_PARSER_HH

#include "lily-proto.hh"
#include "string.hh"
#include "parray.hh"
#include "lily-proto.hh"
#include "lily-proto.hh"
#include "duration.hh"
#include "pitch.hh"
#include "string.hh"
#include "array.hh"
#include "input.hh"

/**
   State for the parser.  Do not ever add any variables to parse
   musical content here.  We still have to remove default_duration_.

   TODO: interface is too complicated
*/
class My_lily_parser 
{
public:
  My_lily_parser (Sources * sources_l);
  ~My_lily_parser ();

  void do_init_file ();
  void parse_file ( String init_str, String file_str);
  void set_version_check (bool ignore);

public:
  Duration default_duration_;

  Scope *default_header_p_;

  int fatal_error_i_;
  Sources * source_l_;
  int error_level_i_;

  My_lily_lexer * lexer_p_;
  bool ignore_version_b_;
  
  Input here_input () const;
  void push_spot ();
  Input pop_spot ();
    
  void do_yyparse ();
  void parser_error (String);

  void set_yydebug (bool);


  DECLARE_SCHEME_CALLBACK (paper_description, ());
private:

  Array<Input> define_spot_array_;

  char const* here_ch_C () const;

  Simultaneous_music * get_chord (Pitch tonic, Array<Pitch>* add_arr_p,
				  Array<Pitch>* sub_arr_p, Pitch* inversion_p,
				  Pitch* bass_p, Duration d);
  
  void set_chord_tremolo (int type_i);
  void set_last_duration (Duration const *);
  void set_last_pitch (Pitch const *);
  friend int yyparse (void*);
};

#endif // MY_LILY_PARSER_HH
