/*
  my-lily-parser.hh -- declare My_lily_parser

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  My_lily_parser (Sources * sources);
  ~My_lily_parser ();

  void do_init_file ();
  void parse_file ( String init_string, String file_string);
  void set_version_check (bool ignore);

public:
  Duration default_duration_;

  Scheme_hash_table *default_header_;

  int fatal_error_;
  Sources * source_;
  int error_level_;

  My_lily_lexer * lexer_;
  bool ignore_version_b_;

  SCM last_beam_start_;
  void beam_check (SCM); 

  Input here_input () const;
  void push_spot ();
  Input pop_spot ();
    
  void do_yyparse ();
  void parser_error (String);

  void set_yydebug (bool);


  DECLARE_SCHEME_CALLBACK (paper_description, ());
private:

  Array<Input> define_spots_;

  char const* here_str0 () const;

  Simultaneous_music * get_chord (Pitch tonic, Array<Pitch>* adds,
				  Array<Pitch>* subs, Pitch* inversion,
				  Pitch* bass, Duration d);
  
  void set_chord_tremolo (int type_i);
  void set_last_duration (Duration const *);
  void set_last_pitch (Pitch const *);
  friend int yyparse (void*);
};

#endif // MY_LILY_PARSER_HH
