/*
  lily-parser.cc -- implement Lily_parser

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
       Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "book.hh"
#include "file-name.hh"
#include "file-path.hh"
#include "lily-version.hh"
#include "ly-module.hh"
#include "main.hh"
#include "lily-lexer.hh"
#include "lily-parser.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "parser.hh"
#include "score.hh"
#include "source.hh"
#include "warn.hh"


Lily_parser::Lily_parser (Sources *sources)
{
  book_count_ = 0;
  score_count_ = 0;
  lexer_ = 0;
  sources_ = sources;
  default_duration_ = Duration (2, 0);
  error_level_ = 0;
  last_beam_start_ = SCM_EOL;

  smobify_self ();
}

Lily_parser::Lily_parser (Lily_parser const &src)
{
  book_count_ = src.book_count_;
  score_count_ = src.score_count_;
  lexer_ = 0;
  sources_ = src.sources_;
  default_duration_ = src.default_duration_;
  error_level_ = src.error_level_;
  last_beam_start_ = src.last_beam_start_;

  smobify_self ();
  if (src.lexer_)
    lexer_ = new Lily_lexer (*src.lexer_);

  scm_gc_unprotect_object (lexer_->self_scm ());
}

Lily_parser::~Lily_parser ()
{
}

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Lily_parser);
IMPLEMENT_TYPE_P (Lily_parser, "ly:lily-parser?");
IMPLEMENT_DEFAULT_EQUAL_P (Lily_parser);

SCM
Lily_parser::mark_smob (SCM s)
{
  Lily_parser *parser = (Lily_parser*) SCM_CELL_WORD_1 (s);
  return (parser->lexer_) ? parser->lexer_->self_scm () : SCM_EOL;
}

int
Lily_parser::print_smob (SCM s, SCM port, scm_print_state*)
{
  scm_puts ("#<my_lily_parser ", port);
  Lily_parser *parser = (Lily_parser*) SCM_CELL_WORD_1 (s);
  (void) parser;
  scm_puts (" >", port);
  return 1;
}


/* Process one .ly file, or book.  */
void
Lily_parser::parse_file (String init, String name, String out_name)
{
  lexer_ = new Lily_lexer (sources_);
  scm_gc_unprotect_object (lexer_->self_scm ());
  // TODO: use $parser 
  lexer_->set_identifier (ly_symbol2scm ("parser"),
			  self_scm ());
  output_basename_ = out_name;
  
  lexer_->main_input_name_ = name;

  progress_indication (_ ("Parsing..."));
  progress_indication ("\n");

  set_yydebug (0);

  lexer_->new_input (init, sources_);

  /* Read .ly IN_FILE, lex, parse, write \score blocks from IN_FILE to
     OUT_FILE (unless IN_FILE redefines output file name).  */
  do_yyparse ();
  
  if (!define_spots_.is_empty ())
    {
      define_spots_.top ().warning (_ ("Braces don't match"));
      error_level_ = 1;
    }

  error_level_ = error_level_ | lexer_->error_level_;
  lexer_ = 0;
}

void
Lily_parser::parse_string (String ly_code)
{
  Lily_lexer * parent = lexer_;
  lexer_ = (parent == 0 ? new Lily_lexer (sources_)
	    : new Lily_lexer (*parent));
  scm_gc_unprotect_object (lexer_->self_scm ());


  SCM oldmod = scm_current_module ();
  scm_set_current_module (scm_car (lexer_->scopes_));
  
  // TODO: use $parser 
  lexer_->set_identifier (ly_symbol2scm ("parser"),
			  self_scm ());
  
  lexer_->main_input_name_ = "<string>";
  lexer_->main_input_b_ = true;

  set_yydebug (0);
  lexer_->new_input (lexer_->main_input_name_, ly_code, sources_);
  do_yyparse ();
  
  if (!define_spots_.is_empty ())
    {
      define_spots_.top ().warning (_ ("Braces don't match"));
      error_level_ = 1;
    }

  error_level_ = error_level_ | lexer_->error_level_;

  scm_set_current_module (oldmod);
  lexer_ = 0;
}

void
Lily_parser::push_spot ()
{
  define_spots_.push (here_input ());
}

char const *
Lily_parser::here_str0 () const
{
  return lexer_->here_str0 ();
}

void
Lily_parser::parser_error (String s)
{
  here_input ().error (s);
  error_level_ = 1;
}

Input
Lily_parser::pop_spot ()
{
  return define_spots_.pop ();
}

Input
Lily_parser::here_input () const
{
  /*
    Parsing looks ahead , so we really want the previous location of the
    lexer, not lexer_->here_input ().
  */
  
  /*
    Actually, that gets very icky when there are white space, because
    the line-numbers are all wrong.  Let's try the character before
    the current token. That gets the right result for note/duration
    stuff, but doesn't mess up for errors in the 1st token of the
    line.
  */
  Input hi (lexer_->here_input ());

  char const * bla = hi.defined_str0_;
  if (hi.line_number () > 1
      || hi.column_number () > 1)
    bla --;
  
  return Input (hi.source_file_, bla);
}


/****************************************************************/


/*
  junkme?
 */
bool store_locations_global_b;

/* Do not append `!' suffix, since 1st argument is not modified. */
LY_DEFINE (ly_set_point_and_click, "ly:set-point-and-click",
	   1, 0, 0, (SCM what),
	  "Set the options for Point-and-click source specials output. The\n"
"argument is a symbol.  Possible options are @code{none} (no source specials),\n"
"@code{line} and @code{line-column}")
{
  /* UGH. */
  SCM val = SCM_BOOL_F;
  if (ly_symbol2scm ("line-column") == what)
    val = ly_scheme_function ("line-column-location");
  else if (what == ly_symbol2scm ("line"))
    val = ly_scheme_function ("line-location");

  scm_module_define (global_lily_module, ly_symbol2scm ("point-and-click"),
		     val);
  store_locations_global_b = ly_c_procedure_p (val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_parse_file, "ly:parse-file",
	   1, 0, 0, (SCM name),
	   "Parse a single @code{.ly} file.  "
	   "Upon failure, throw @code{ly-file-failed} key.")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");
  char const *file = scm_i_string_chars (name);
  char const *extensions[] = {"ly", "", 0};

  String file_name = global_path.find (file, extensions);

  /* By default, use base name of input file for output file name,
     write output to cwd; do not use root and directory parts of input
     file name.  */
  File_name out_file_name (file_name);

  global_path.append (out_file_name.dir_);
  
  out_file_name.ext_ = "";
  out_file_name.root_ = "";
  out_file_name.dir_ = "";

  if (!output_name_global.is_empty ())
    out_file_name = File_name (output_name_global);
  
  String init;
  if (!init_name_global.is_empty ())
    init = init_name_global;
  else
    init = "init.ly";

  String out_file = out_file_name.to_string ();

  if (init.length () && global_path.find (init).is_empty ())
    {
      warning (_f ("can't find init file: `%s'", init));
      warning (_f ("(search path: `%s')",
		   global_path.to_string ().to_str0 ()));
      exit (2);
    }

  if ((file_name != "-") && global_path.find (file_name).is_empty ())
    {
      warning (_f ("can't find file: `%s'", file_name));
      scm_throw (ly_symbol2scm ("ly-file-failed"),
		 scm_list_1 (scm_makfrom0str (file_name.to_str0 ())));
    }
  else
    {
      Sources sources;
      sources.set_path (&global_path);
  
      progress_indication (_f ("Processing `%s'", file_name.to_str0 ()));
      progress_indication ("\n");

      Lily_parser *parser = new Lily_parser (&sources);

      parser->parse_file (init, file_name, out_file);

      bool error = parser->error_level_;
      scm_gc_unprotect_object (parser->self_scm ());
      parser = 0;
      if (error)
	/* TODO: pass renamed input file too.  */
	scm_throw (ly_symbol2scm ("ly-file-failed"),
		   scm_list_1 (scm_makfrom0str (file_name.to_str0 ())));
    }
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_parse_string, "ly:parse-string",
	   1, 0, 0, (SCM ly_code),
	   "Parse the string LY_CODE.  "
	   "Upon failure, throw @code{ly-file-failed} key.")
{
  SCM_ASSERT_TYPE (scm_is_string (ly_code), ly_code, SCM_ARG1, __FUNCTION__, "string");
  
  Sources sources;
  sources.set_path (&global_path);
  Lily_parser *parser = new Lily_parser (&sources);
  scm_module_define (global_lily_module, ly_symbol2scm ("parser"),
		     parser->self_scm ());
  parser->parse_string (ly_scm2string (ly_code));
  scm_gc_unprotect_object (parser->self_scm ());
  parser = 0;
  
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_clone_parser, "ly:clone-parser",
           1, 0, 0, (SCM parser_smob),
           "Return a clone of PARSER_SMOB.")
{
  Lily_parser *parser = unsmob_my_lily_parser (parser_smob);
  Lily_parser *clone = new Lily_parser (*parser);

  /* FIXME: should copy scopes too. */
  return scm_gc_unprotect_object (clone->self_scm ());
}

LY_DEFINE (ly_parser_define, "ly:parser-define",
	   3, 0, 0, (SCM parser_smob, SCM symbol, SCM val),
	   "Bind SYMBOL to VAL in PARSER_SMOB's module.")
{
  Lily_parser *parser = unsmob_my_lily_parser (parser_smob);
  SCM_ASSERT_TYPE (scm_is_symbol (symbol), symbol, SCM_ARG2, __FUNCTION__, "symbol");
  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG2, __FUNCTION__, "parser");  

  parser->lexer_->set_identifier (scm_symbol_to_string (symbol), val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_parser_lookup, "ly:parser-lookup",
	   2, 0, 0, (SCM parser_smob, SCM symbol),
	   "Lookup @var{symbol} in @var{parser_smob}'s module.  "
	   "Undefined is '().")
{
  Lily_parser *parser = unsmob_my_lily_parser (parser_smob);

  SCM_ASSERT_TYPE (scm_is_symbol (symbol), symbol, SCM_ARG2, __FUNCTION__, "symbol");
  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG2, __FUNCTION__, "parser");  

  SCM val= parser->lexer_->lookup_identifier (ly_scm2string (scm_symbol_to_string (symbol)));
  if (val != SCM_UNDEFINED)
    return val;
  else
    return SCM_EOL;
}

LY_DEFINE (ly_parser_parse_string, "ly:parser-parse-string",
	   2, 0, 0, (SCM parser_smob, SCM ly_code),
	   "Parse the string LY_CODE with PARSER_SMOB."
	   "Upon failure, throw @code{ly-file-failed} key.")
{
  Lily_parser *parser = unsmob_my_lily_parser (parser_smob);

  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG1, __FUNCTION__, "parser");
  SCM_ASSERT_TYPE (scm_is_string (ly_code), ly_code, SCM_ARG2, __FUNCTION__, "string");
  
  parser->parse_string (ly_scm2string (ly_code));
  
  return SCM_UNSPECIFIED;
}

Output_def*
get_layout (Lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier ("$defaultlayout");
  Output_def *layout = unsmob_output_def (id);
  layout = layout ? layout->clone () : new Output_def;
  layout->set_variable (ly_symbol2scm ("is-layout"), SCM_BOOL_T);

  return layout;
}


Output_def*
get_midi (Lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier ("$defaultmidi");
  Output_def *layout = unsmob_output_def (id);
  layout = layout ? layout->clone () : new Output_def;
  layout->set_variable (ly_symbol2scm ("is-midi"), SCM_BOOL_T);
  return layout;
}


Output_def*
get_paper (Lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier ("$defaultpaper");
  Output_def *layout = unsmob_output_def (id);

  layout = layout ? dynamic_cast<Output_def*> (layout->clone ()) : new Output_def;
  layout->set_variable (ly_symbol2scm ("is-paper"), SCM_BOOL_T);
  return layout;
}


/* TODO: move this to Scheme?  Why take the parser arg, and all the back
   & forth between scm and c++? */
LY_DEFINE (ly_parser_print_score, "ly:parser-print-score",
	   2, 0, 0,
	   (SCM parser_smob, SCM score_smob),
	   "Print score, i.e., the classic way.")
{
  Lily_parser *parser = unsmob_my_lily_parser (parser_smob);
  Score *score = unsmob_score (score_smob);

  if (score->error_found_)
    return SCM_UNSPECIFIED;
  
  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG1, __FUNCTION__, "parser");
  SCM_ASSERT_TYPE (score, score_smob, SCM_ARG2, __FUNCTION__, "score");

  SCM header = ly_c_module_p (score->header_) ? score->header_
    : parser->lexer_->lookup_identifier ("$globalheader");
  
  File_name outname (parser->output_basename_);
  int *c = &parser->book_count_;
  if (*c)
    outname.base_ += "-" + to_string (*c);
  (*c)++;

  SCM os = scm_makfrom0str (outname.to_string ().to_str0 ());
  SCM paper = get_paper (parser)->self_scm ();
  for (int i = 0; i < score->defs_.size (); i++)
    default_rendering (score->get_music (), score->defs_[i]->self_scm (),
		       paper,
		       header, os);

  if (score->defs_.is_empty ())
    {
      Output_def *layout = get_layout (parser);
      default_rendering (score->get_music(), layout->self_scm (),
			 get_paper (parser)->self_scm (),
			 header, os);
      scm_gc_unprotect_object (layout->self_scm ());
    }
  return SCM_UNSPECIFIED;
}


LY_DEFINE (ly_parser_set_note_names, "ly:parser-set-note-names",
	   2, 0, 0, (SCM parser, SCM names),
	   "Replace current note names in @var{parser}. "
	   "@var{names} is an alist of symbols.  "
	   "This only has effect if the current mode is notes.")
{
  Lily_parser *p = unsmob_my_lily_parser (parser);
  SCM_ASSERT_TYPE(p, parser, SCM_ARG1, __FUNCTION__, "Lilypond parser");

  if (p->lexer_->is_note_state ())
    {
      p->lexer_->pop_state ();
      p->lexer_->push_note_state (alist_to_hashq (names));
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_parser_print_book, "ly:parser-print-book",
	   2, 0, 0, (SCM parser_smob, SCM book_smob),
	   "Print book.")
{
  Lily_parser *parser = unsmob_my_lily_parser (parser_smob);
  Book *book = unsmob_book (book_smob);
  Output_def *bp = unsmob_output_def (parser->lexer_->lookup_identifier ("$defaultpaper"));
  
  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG1, __FUNCTION__, "Lilypond parser");
  SCM_ASSERT_TYPE (book, book_smob, SCM_ARG2, __FUNCTION__, "Book");
  
  /*  ugh. changing argument.*/
  book->paper_ = bp;
  
  File_name outname (parser->output_basename_);
  int *c = &parser->book_count_;
  if (*c)
    outname.base_ += "-" + to_string (*c);
  (*c)++;

  Output_def *layout = get_layout (parser);

  Paper_book* pb = book->process (outname.to_string (), layout);
  if (pb)
    {
      pb->output (outname.to_string ());
      scm_gc_unprotect_object (pb->self_scm ());
    }

  scm_gc_unprotect_object (layout->self_scm ());
  return SCM_UNSPECIFIED;
}

