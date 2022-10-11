/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
   State for the parser.

   TODO: interface is too complicated
*/
class Lily_parser : public Smob<Lily_parser>
{
  SCM do_yyparse ();
  static SCM do_yyparse_trampoline (void *parser);

public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Lily_parser ();
  Lily_lexer *lexer_;
  Sources *sources_;
  Duration default_duration_;
  int default_tremolo_type_;
  std::string output_basename_;

  // See ly:parser-clone. TODO: are the offsets in chars or in bytes?
  SCM closures_;

  int fatal_error_;
  int error_level_;
  bool ignore_version_b_;

  Lily_parser (Sources *sources);
  Lily_parser (Lily_parser const &, SCM closures = SCM_EOL,
               SCM location = SCM_BOOL_F);

  DECLARE_SCHEME_CALLBACK (layout_description, ());

  void clear ();
  void do_init_file ();
  void include_string (const std::string &ly_code);
  void parse_file (const std::string &init, const std::string &name,
                   const std::string &out_name);
  void parse_string (const std::string &ly_code);
  SCM parse_string_expression (const std::string &ly_code,
                               const std::string &filename, int line);
  void parser_error (const std::string &);
  void parser_error (Input const &, const std::string &);
  // The following is called as yyerror
  static void parser_error (Input const *i, Lily_parser *parser, SCM *,
                            const std::string &s);
  void set_yydebug (bool);
};

Output_def *get_layout (Lily_parser *parser);
Output_def *get_midi (Lily_parser *parser);
Output_def *get_paper (Lily_parser *parser);
void init_papers (Lily_parser *parser);
void push_paper (Lily_parser *parser, Output_def *paper);
void pop_paper (Lily_parser *parser);
void set_paper (Lily_parser *parser, Output_def *paper);
SCM get_header (Lily_parser *parser);

#endif /* LILY_PARSER_HH */
