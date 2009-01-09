/*
  lily-parser.cc -- implement Lily_parser

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lily-parser.hh"

#include "book.hh"
#include "file-name.hh"
#include "file-path.hh"
#include "international.hh"
#include "lily-lexer.hh"
#include "lily-version.hh"
#include "ly-module.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "parser.hh"
#include "score.hh"
#include "sources.hh"
#include "warn.hh"
#include "program-option.hh"

#include "ly-smobs.icc"

Lily_parser::Lily_parser (Sources *sources)
{
  lexer_ = 0;
  sources_ = sources;
  default_duration_ = Duration (2, 0);
  error_level_ = 0;

  smobify_self ();

  lexer_ = new Lily_lexer (sources_, this);
  lexer_->unprotect ();
}

Lily_parser::Lily_parser (Lily_parser const &src)
{
  lexer_ = 0;
  sources_ = src.sources_;
  default_duration_ = src.default_duration_;
  error_level_ = src.error_level_;
  output_basename_ = src.output_basename_;

  smobify_self ();
  if (src.lexer_)
    {
      lexer_ = new Lily_lexer (*src.lexer_, this);
    }
  
  lexer_->unprotect ();
}

Lily_parser::~Lily_parser ()
{
}


SCM
Lily_parser::mark_smob (SCM s)
{
  Lily_parser *parser = (Lily_parser *) SCM_CELL_WORD_1 (s);
  return (parser->lexer_) ? parser->lexer_->self_scm () : SCM_EOL;
}

int
Lily_parser::print_smob (SCM s, SCM port, scm_print_state*)
{
  scm_puts ("#<Lily_parser ", port);
  Lily_parser *parser = (Lily_parser *) SCM_CELL_WORD_1 (s);
  if (parser->lexer_)
    scm_display (parser->lexer_->self_scm (), port);
  else
    scm_puts ("(no lexer yet)", port);
  scm_puts (" >", port);
  return 1;
}

/* Process one .ly file, or book.  */
void
Lily_parser::parse_file (string init, string name, string out_name)
{
  // TODO: use $parser 
  lexer_->set_identifier (ly_symbol2scm ("parser"), self_scm ());
  output_basename_ = out_name;

  lexer_->main_input_name_ = name;

  message (_ ("Parsing..."));

  set_yydebug (0);

  lexer_->new_input (init, sources_);

  File_name f (name);
  string s = global_path.find (f.base_ + ".twy");
  s = gulp_file_to_string (s, false, -1);
  scm_eval_string (ly_string2scm (s));

  /* Read .ly IN_FILE, lex, parse, write \score blocks from IN_FILE to
     OUT_FILE (unless IN_FILE redefines output file name).  */

  SCM mod = lexer_->set_current_scope ();
  do_yyparse ();

  /*
    Don't mix cyclic pointers with weak tables.
  */
  lexer_->set_identifier (ly_symbol2scm ("parser"),
			  SCM_EOL);
  ly_reexport_module (scm_current_module ());

  scm_set_current_module (mod);

  if (!define_spots_.empty ())
    {
      define_spots_.back ().warning (_ ("braces do not match"));
      error_level_ = 1;
    }

  error_level_ = error_level_ | lexer_->error_level_;
  clear ();
}

void
Lily_parser::parse_string (string ly_code)
{
  // TODO: use $parser 
  lexer_->set_identifier (ly_symbol2scm ("parser"),
			  self_scm ());

  lexer_->main_input_name_ = "<string>";
  lexer_->is_main_input_ = true; 
  lexer_->new_input (lexer_->main_input_name_, ly_code, sources_);

  SCM mod = lexer_->set_current_scope ();
  do_yyparse ();
  scm_set_current_module (mod);

  if (!define_spots_.empty ())
    {
      if (define_spots_.empty ()
	  && !error_level_)
	programming_error ("define_spots_ don't match, but error_level_ not set.");
    }

  error_level_ = error_level_ | lexer_->error_level_;
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

char const *
Lily_parser::here_str0 () const
{
  return lexer_->here_str0 ();
}

void
Lily_parser::parser_error (string s)
{
  lexer_->here_input ().error (_ (s.c_str ()));
  error_level_ = 1;
}

void
Lily_parser::parser_error (Input const &i, string s)
{
  i.error (s);
  error_level_ = 1;
}



IMPLEMENT_SMOBS (Lily_parser);
IMPLEMENT_TYPE_P (Lily_parser, "ly:lily-parser?");
IMPLEMENT_DEFAULT_EQUAL_P (Lily_parser);


/****************************************************************
  OUTPUT-DEF 
 ****************************************************************/

Output_def *
get_layout (Lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier ("$defaultlayout");
  Output_def *layout = unsmob_output_def (id);
  layout = layout ? layout->clone () : new Output_def;
  layout->set_variable (ly_symbol2scm ("is-layout"), SCM_BOOL_T);
    
  return layout;
}

Output_def *
get_midi (Lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier ("$defaultmidi");
  Output_def *layout = unsmob_output_def (id);
  layout = layout ? layout->clone () : new Output_def;
  layout->set_variable (ly_symbol2scm ("is-midi"), SCM_BOOL_T);
  return layout;
}

/* Return a copy of the top of $papers stack, or $defaultpaper if the
 * stack is empty */
Output_def *
get_paper (Lily_parser *parser)
{
  SCM papers = parser->lexer_->lookup_identifier ("$papers");
  Output_def *layout = ((papers == SCM_UNDEFINED) || scm_is_null (papers)) ?
    0 : unsmob_output_def (scm_car (papers));
  SCM default_paper = parser->lexer_->lookup_identifier ("$defaultpaper");
  layout = layout ? layout : unsmob_output_def (default_paper);

  layout = layout ? dynamic_cast<Output_def *> (layout->clone ()) : new Output_def;
  layout->set_variable (ly_symbol2scm ("is-paper"), SCM_BOOL_T);
  return layout;
}

/* Initialize (reset) the $papers stack */
void
init_papers (Lily_parser *parser)
{
  parser->lexer_->set_identifier (ly_symbol2scm ("$papers"), SCM_EOL);
}

/* Push a paper on top of $papers stack */
void
push_paper (Lily_parser *parser, Output_def *paper)
{
  parser->lexer_->set_identifier (ly_symbol2scm ("$papers"),
                                  scm_cons (paper->self_scm (),
                                            parser->lexer_->lookup_identifier ("$papers")));
}

/* Pop a paper from $papers stack */
void
pop_paper (Lily_parser *parser)
{
  if (! scm_is_null (parser->lexer_->lookup_identifier ("$papers")))
    parser->lexer_->set_identifier (ly_symbol2scm ("$papers"),
                                    scm_cdr (parser->lexer_->lookup_identifier ("$papers")));
}

/* Change the paper on top of $papers stack */
void
set_paper (Lily_parser *parser, Output_def *paper)
{
  scm_set_car_x (parser->lexer_->lookup_identifier ("$papers"), paper->self_scm ());
}

SCM
get_header (Lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier ("$defaultheader");
  if (!ly_is_module (id))
    id = parser->make_scope ();
  else
    {
      SCM nid = parser->make_scope ();
      ly_module_copy (nid, id);
      id = nid;
    }
  
  return id;
}

SCM 
Lily_parser::make_scope () const
{
  SCM module = ly_make_anonymous_module (be_safe_global);
  return module;    
}
