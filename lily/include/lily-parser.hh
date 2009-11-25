/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef LILY_PARSER_HH
#define LILY_PARSER_HH

#include "duration.hh"
#include "input.hh"
#include "pitch.hh"

/**
   State for the parser.  Do not ever add any variables to parse
   musical content here.  We still have to remove default_duration_.

   TODO: interface is too complicated
*/
class Lily_parser
{
  DECLARE_SMOBS (Lily_parser);
  friend int yyparse (void *);

  vector<Input> define_spots_;

  char const *here_str0 () const;
  Simultaneous_music *get_chord (Pitch tonic,
				 vector<Pitch> *adds, vector<Pitch> *subs,
				 Pitch *inversion, Pitch *bass, Duration d);
  void set_chord_tremolo (int type);
  void set_last_duration (Duration const *);
  void set_last_pitch (Pitch const *);

public:
  Lily_lexer *lexer_;
  Sources *sources_;
  Duration default_duration_;
  string output_basename_;

  int fatal_error_;
  int error_level_;
  bool ignore_version_b_;

  Lily_parser (Sources *sources);
  Lily_parser (Lily_parser const &);

  DECLARE_SCHEME_CALLBACK (layout_description, ());

  void clear ();
  void do_init_file ();
  void do_yyparse ();
  void parse_file (string init, string name, string out_name);
  void parse_string (string ly_code);
  void parser_error (string);
  void parser_error (Input const &, string);
  void set_yydebug (bool);

  SCM make_scope () const; 
};

DECLARE_UNSMOB (Lily_parser, lily_parser);

SCM ly_parse_file (SCM);
SCM ly_parse_string (SCM);
// SCM ly_parser_add_book_and_score (SCM, SCM);
SCM ly_parser_print_book (SCM, SCM);
SCM ly_parser_print_score (SCM, SCM);
SCM ly_parser_bookify (SCM, SCM);
SCM ly_parser_scorify (SCM, SCM);

Output_def *get_layout (Lily_parser *parser);
Output_def *get_midi (Lily_parser *parser);
Output_def *get_paper (Lily_parser *parser);
void init_papers (Lily_parser *parser);
void push_paper (Lily_parser *parser, Output_def *paper);
void pop_paper (Lily_parser *parser);
void set_paper (Lily_parser *parser, Output_def *paper);
SCM get_header (Lily_parser *parser);

#endif /* LILY_PARSER_HH */
