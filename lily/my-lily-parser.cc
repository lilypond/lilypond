/*
  my-lily-parser.cc -- implement My_lily_parser

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
#include "my-lily-lexer.hh"
#include "my-lily-parser.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "parser.hh"
#include "score.hh"
#include "source.hh"
#include "warn.hh"

#include "ly-smobs.icc"


My_lily_parser::My_lily_parser (Sources *sources)
{
  book_count_ = 0;
  score_count_ = 0;
  lexer_ = 0;
  sources_ = sources;
  default_duration_ = Duration (2,0);
  error_level_ = 0;
  last_beam_start_ = SCM_EOL;

  smobify_self ();
}

My_lily_parser::My_lily_parser (My_lily_parser const &src)
{
  book_count_ = src.book_count_;
  score_count_ = src.score_count_;
  lexer_ = 0;
  sources_ = src.sources_;
  default_duration_ = src.default_duration_;
  error_level_ = src.error_level_;
  last_beam_start_ = src.last_beam_start_;

  smobify_self ();
  lexer_ = new My_lily_lexer (*src.lexer_);
}

My_lily_parser::~My_lily_parser ()
{
  // FIXME: Memleak: del lexer
}

IMPLEMENT_SMOBS (My_lily_parser);
IMPLEMENT_TYPE_P (My_lily_parser, "ly:my-lily-parser?");
IMPLEMENT_DEFAULT_EQUAL_P (My_lily_parser);

SCM
My_lily_parser::mark_smob (SCM s)
{
  My_lily_parser *parser = (My_lily_parser*) ly_cdr (s);
  return SCM_EOL;
}

int
My_lily_parser::print_smob (SCM s, SCM port, scm_print_state*)
{
  scm_puts ("#<my_lily_parser ", port);
  My_lily_parser *parser = (My_lily_parser*) ly_cdr (s);
  (void) parser;
  scm_puts (" >", port);
  return 1;
}


/* Process one .ly file, or book.  */
void
My_lily_parser::parse_file (String init, String name, String out_name)
{
  lexer_ = new My_lily_lexer (sources_);
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
  delete lexer_;
  lexer_ = 0;
}

void
My_lily_parser::parse_string (String ly_code)
{
  My_lily_lexer *parent = lexer_;
  lexer_ = (parent == 0 ? new My_lily_lexer (sources_)
	    : new My_lily_lexer (*parent));

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

  if (parent != 0)
    {
      parent->keytable_ = lexer_->keytable_;
      parent->encoding_ = lexer_->encoding_;
      parent->chordmodifier_tab_ = lexer_->chordmodifier_tab_;
      parent->pitchname_tab_stack_ = lexer_->pitchname_tab_stack_;
      parent->sources_ = lexer_->sources_;
      parent->scopes_ = lexer_->scopes_;
      parent->error_level_ = lexer_->error_level_; 
      parent->main_input_b_ = lexer_->main_input_b_;
    }

  delete lexer_;
  lexer_ = 0;
}

void
My_lily_parser::push_spot ()
{
  define_spots_.push (here_input ());
}

char const *
My_lily_parser::here_str0 () const
{
  return lexer_->here_str0 ();
}

void
My_lily_parser::parser_error (String s)
{
  here_input ().error (s);
  error_level_ = 1;
}

Input
My_lily_parser::pop_spot ()
{
  return define_spots_.pop ();
}

Input
My_lily_parser::here_input () const
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
LY_DEFINE (ly_set_point_and_click, "ly:set-point-and-click", 1, 0, 0,
	  (SCM what),
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
	   1, 0, 0,
	   (SCM name),
	   "Parse a single @code{.ly} file.  "
	   "Upon failure, throw @code{ly-file-failed} key.")
{
  SCM_ASSERT_TYPE (ly_c_string_p (name), name, SCM_ARG1, __FUNCTION__, "string");
  char const *file = SCM_STRING_CHARS (name);
  char const *extensions[] = {"ly", "", 0};
  String file_name = global_path.find (file, extensions);
  
  /* By default, use base name of input file for output file name,
     write output to cwd; do not use root and directory parts of input
     file name.  */
  File_name out_file_name (file_name);
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
  
      progress_indication (_f ("Now processing `%s'", file_name.to_str0 ()));
      progress_indication ("\n");

      My_lily_parser *parser = new My_lily_parser (&sources);

      // TODO: use $parser 
      scm_module_define (global_lily_module, ly_symbol2scm ("parser"),
			 parser->self_scm ());
      parser->parse_file (init, file_name, out_file);

      bool error = parser->error_level_;
      parser = 0;
      if (error)
	/* TODO: pass renamed input file too.  */
	scm_throw (ly_symbol2scm ("ly-file-failed"),
		   scm_list_1 (scm_makfrom0str (file_name.to_str0 ())));
    }
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_parse_string, "ly:parse-string",
	   1, 0, 0,
	   (SCM ly_code),
	   "Parse the string LY_CODE.  "
	   "Upon failure, throw @code{ly-file-failed} key.")
{
  SCM_ASSERT_TYPE (ly_c_string_p (ly_code), ly_code, SCM_ARG1, __FUNCTION__, "string");
  
  Sources sources;
  sources.set_path (&global_path);
  My_lily_parser *parser = new My_lily_parser (&sources);
  scm_module_define (global_lily_module, ly_symbol2scm ("parser"),
		     parser->self_scm ());
  parser->parse_string (ly_scm2string (ly_code));
  parser = 0;
  
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_clone_parser, "ly:clone-parser",
           1, 0, 0, 
           (SCM parser_smob),
           "Return a clone of PARSER_SMOB.")
{
  My_lily_parser *parser = unsmob_my_lily_parser (parser_smob);
  My_lily_parser *clone = new My_lily_parser (*parser);

  /*
    FIXME: should copy scopes too.
   */
  return scm_gc_unprotect_object (clone->self_scm ());
}

LY_DEFINE(ly_parser_define, "ly:parser-define",
          3, 0, 0, 
          (SCM parser_smob, SCM symbol, SCM val),
          "Bind SYMBOL to VAL in PARSER_SMOB's module.")
{
  My_lily_parser *parser = unsmob_my_lily_parser (parser_smob);
  SCM_ASSERT_TYPE (ly_c_symbol_p (symbol), symbol, SCM_ARG2, __FUNCTION__, "symbol");
  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG2, __FUNCTION__, "parser");  


  parser->lexer_->set_identifier (scm_symbol_to_string (symbol), val);
  return SCM_UNSPECIFIED;
}
LY_DEFINE(ly_parser_lookup, "ly:parser-lookup",
          2, 0, 0, 
          (SCM parser_smob, SCM symbol),
          "Lookup @var{symbol} in @var{parser_smob}'s module. Undefined is '().")
{
  My_lily_parser *parser = unsmob_my_lily_parser (parser_smob);

  SCM_ASSERT_TYPE (ly_c_symbol_p (symbol), symbol, SCM_ARG2, __FUNCTION__, "symbol");
  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG2, __FUNCTION__, "parser");  

  SCM val= parser->lexer_->lookup_identifier (ly_scm2string (scm_symbol_to_string (symbol)));
  if (val != SCM_UNDEFINED)
    return val;
  else
    return SCM_EOL;
}

LY_DEFINE (ly_parser_parse_string, "ly:parser-parse-string",
	   2, 0, 0,
	   (SCM parser_smob, SCM ly_code),
	   "Parse the string LY_CODE with PARSER_SMOB."
	   "Upon failure, throw @code{ly-file-failed} key.")
{

  My_lily_parser *parser = unsmob_my_lily_parser (parser_smob);

  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG1, __FUNCTION__, "parser");
  SCM_ASSERT_TYPE (ly_c_string_p (ly_code), ly_code, SCM_ARG2, __FUNCTION__, "string");
  
  parser->parse_string (ly_scm2string (ly_code));
  
  return SCM_UNSPECIFIED;
}

Output_def*
get_paper (My_lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier ("$defaultpaper");
  Output_def *paper = unsmob_output_def (id);
  paper = paper ? paper->clone () : new Output_def;
  paper->set_variable (ly_symbol2scm ("is-paper"), SCM_BOOL_T);

  paper->parent_ =unsmob_output_def (parser->lexer_->lookup_identifier ("$defaultbookpaper"));
  return paper;
}


Output_def*
get_midi (My_lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier ("$defaultmidi");
  Output_def *paper = unsmob_output_def (id);
  paper = paper ? paper->clone () : new Output_def;
  paper->set_variable (ly_symbol2scm ("is-midi"), SCM_BOOL_T);
  return paper;
}


Output_def*
get_bookpaper (My_lily_parser *parser)
{
  SCM id = parser->lexer_->lookup_identifier ("$defaultbookpaper");
  Output_def *paper = unsmob_output_def (id);

  paper = paper ? dynamic_cast<Output_def*> (paper->clone ()) : new Output_def;
  paper->set_variable (ly_symbol2scm ("is-bookpaper"), SCM_BOOL_T);
  return paper;
}


/*
  TODO: move this to Scheme? Why take the parser arg, and all the back
  & forth between scm and c++?
 */
LY_DEFINE (ly_parser_print_score, "ly:parser-print-score",
	   2, 0, 0,
	   (SCM parser_smob, SCM score_smob),
	   "Print score, i.e., the classic way.")
{
  My_lily_parser *parser = unsmob_my_lily_parser (parser_smob);
  Score *score = unsmob_score (score_smob);

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
  SCM bookpaper = get_bookpaper (parser)->self_scm ();
  for (int i = 0; i < score->defs_.size (); i++)
    default_rendering (score->music_, score->defs_[i]->self_scm (),
		       bookpaper,
		       header, os);

  if (score->defs_.is_empty ())
    {
      Output_def *paper = get_paper (parser);
      default_rendering (score->music_, paper->self_scm (),
			 get_bookpaper (parser)->self_scm (),
			 header, os);
      scm_gc_unprotect_object (paper->self_scm ());
    }
  return SCM_UNDEFINED;
}


LY_DEFINE (ly_parser_set_names, "ly:parser-set-note-names",
	   2, 0, 0,
	   (SCM parser, SCM names),
	   "Replace current note names in @var{parser}. @var{names} is an alist of "
	   "symbols. This only has effect if the current mode is notes.")
{
  My_lily_parser *p = unsmob_my_lily_parser (parser);
  SCM_ASSERT_TYPE(p, parser, SCM_ARG1, __FUNCTION__, "Lilypond parser");

  if (p->lexer_->is_note_state ())
    {
      p->lexer_->pop_state ();
      p->lexer_->push_note_state (alist_to_hashq (names));
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_parser_print_book, "ly:parser-print-book",
	   2, 0, 0,
	   (SCM parser_smob, SCM book_smob),
	   "Print book.")
{
  My_lily_parser *parser = unsmob_my_lily_parser (parser_smob);
  Book *book = unsmob_book (book_smob);
  Output_def *bp = unsmob_output_def (parser->lexer_->lookup_identifier ("$defaultbookpaper"));
  
  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG1, __FUNCTION__, "Lilypond parser");
  SCM_ASSERT_TYPE (book, book_smob, SCM_ARG2, __FUNCTION__, "Book");
  
  /*  ugh. changing argument.*/
  book->bookpaper_ = bp;
  
  File_name outname (parser->output_basename_);
  int *c = &parser->book_count_;
  if (*c)
    outname.base_ += "-" + to_string (*c);
  (*c)++;

  Output_def *paper = get_paper (parser);

  Paper_book* pb = book->process (outname.to_string (), paper);

  pb->output (outname.to_string ());
  
  scm_gc_unprotect_object (paper->self_scm ());
  scm_gc_unprotect_object (pb->self_scm ());

  return SCM_UNDEFINED;
}

