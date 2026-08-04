/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "lily-parser.hh"

#include "book.hh"
#include "file-name.hh"
#include "file-path.hh"
#include "international.hh"
#include "lily-lexer.hh"
#include "ly-module.hh"
#include "main.hh"
#include "output-def.hh"
#include "parser.hh"
#include "score.hh"
#include "source-file.hh"
#include "sources.hh"
#include "warn.hh"
#include "program-option.hh"

Lily_parser::Lily_parser (Sources *sources)
{
  lexer_ = 0;
  sources_ = sources;
  default_duration_ = Duration (2, 0);
  default_tremolo_type_ = 8;
  error_level_ = 0;
  closures_ = SCM_EOL;

  smobify_self ();

  lexer_ = new Lily_lexer (sources_, this);
  lexer_->unprotect ();
}

Lily_parser::Lily_parser (Lily_parser const &src, SCM closures, SCM location)
  : Smob<Lily_parser> ()
{
  lexer_ = 0;
  sources_ = src.sources_;
  default_duration_ = src.default_duration_;
  default_tremolo_type_ = src.default_tremolo_type_;
  error_level_ = 0;
  output_basename_ = src.output_basename_;
  closures_ = closures;

  smobify_self ();
  if (src.lexer_)
    {
      lexer_ = new Lily_lexer (*src.lexer_, this, location);
      lexer_->unprotect ();
    }
}

Lily_parser::~Lily_parser ()
{
}

SCM
Lily_parser::mark_smob () const
{
  scm_gc_mark (closures_);
  return (lexer_) ? lexer_->self_scm () : SCM_EOL;
}

int
Lily_parser::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Lily_parser ", port);
  if (lexer_)
    scm_display (lexer_->self_scm (), port);
  else
    scm_puts ("(no lexer yet)", port);
  scm_puts (" >", port);
  return 1;
}

/* Process one .ly file, or book.  */
void
Lily_parser::parse_file (const std::string &init, const std::string &name,
                         const std::string &out_name)
{
  output_basename_ = out_name;

  lexer_->main_input_name_ = name;

  set_yydebug (0);

  lexer_->new_input (init, sources_);

  /* Read .ly IN_FILE, lex, parse, write \score blocks from IN_FILE to
     OUT_FILE (unless IN_FILE redefines output file name).  */

  SCM mod = lexer_->set_current_scope ();
  do
    {
      do_yyparse ();
    }
  while (!lexer_->is_clean ());

  scm_set_current_module (mod);

  error_level_ = error_level_ | lexer_->error_level_;
  clear ();
}

void
Lily_parser::parse_string (const std::string &ly_code)
{
  lexer_->main_input_name_ = "<string>";
  lexer_->new_input (lexer_->main_input_name_, ly_code, sources_);

  SCM mod = lexer_->set_current_scope ();

  do_yyparse ();

  scm_set_current_module (mod);

  error_level_ = error_level_ | lexer_->error_level_;
}

SCM
Lily_parser::parse_string_expression (const std::string &ly_code,
                                      const std::string &filename, int line)
{
  lexer_->main_input_name_ = filename;
  lexer_->new_input (lexer_->main_input_name_, ly_code, sources_);
  if (line)
    {
      lexer_->get_source_file ()->set_line (0, line);
    }
  SCM mod = lexer_->set_current_scope ();

  lexer_->push_extra_token (Input (), EMBEDDED_LILY);
  SCM result = do_yyparse ();

  scm_set_current_module (mod);

  error_level_ = error_level_ | lexer_->error_level_;
  return result;
}

void
Lily_parser::include_string (const std::string &ly_code)
{
  lexer_->new_input ("<included string>", ly_code, sources_);
}

void
Lily_parser::clear ()
{
  if (lexer_)
    {
      while (lexer_->has_scope ())
        lexer_->remove_scope ();
    }

  lexer_ = 0;
}

void
Lily_parser::parser_error (const std::string &s)
{
  lexer_->here_input ().non_fatal_error (_ (s.c_str ()));
  error_level_ = 1;
}

void
Lily_parser::parser_error (Input const &i, const std::string &s)
{
  i.non_fatal_error (s);
  error_level_ = 1;
}

const char *const Lily_parser::type_p_name_ = "ly:lily-parser?";

/****************************************************************
  OUTPUT-DEF
 ****************************************************************/

Output_def *
get_layout (Lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier_symbol (
    ly_symbol2scm ("$defaultlayout"));
  Output_def *layout = unsmob<Output_def> (id);
  layout = layout ? layout->clone () : new Output_def;
  // TODO: use a member in the Output_def. Can't do that for now,
  // because the current paper code uses the output def module, not
  // the output def itself.
  layout->set_variable (ly_symbol2scm ("output-def-kind"),
                        ly_symbol2scm ("layout"));

  return layout;
}

Output_def *
get_midi (Lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier_symbol (
    ly_symbol2scm ("$defaultmidi"));
  Output_def *midi = unsmob<Output_def> (id);
  midi = midi ? midi->clone () : new Output_def;
  midi->set_variable (ly_symbol2scm ("output-def-kind"),
                      ly_symbol2scm ("midi"));
  return midi;
}

/* Get $defaultpaper at the current parsing level */
Output_def *
get_paper (Lily_parser *parser)
{
  SCM default_paper = parser->lexer_->lookup_identifier_symbol (
    ly_symbol2scm ("$defaultpaper"));
  Output_def *paper = unsmob<Output_def> (default_paper);
  paper =  paper ? paper->clone () : new Output_def;

  paper->set_variable (ly_symbol2scm ("output-def-kind"),
                        ly_symbol2scm ("paper"));
  return paper;
}

SCM
get_header (Lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier_symbol (
    ly_symbol2scm ("$defaultheader"));
  if (!ly_is_module (id))
    id = ly_make_module ();
  else
    {
      SCM nid = ly_make_module ();
      ly_module_copy (nid, id);
      id = nid;
    }

  return id;
}
