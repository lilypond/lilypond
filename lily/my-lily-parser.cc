/*
  my-lily-parser.cc -- implement My_lily_parser

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
       Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "book.hh"
#include "file-path.hh"
#include "lily-version.hh"
#include "ly-module.hh"
#include "ly-smobs.icc"
#include "main.hh"
#include "my-lily-lexer.hh"
#include "my-lily-parser.hh"
#include "paper-def.hh"
#include "parray.hh"
#include "parser.hh"
#include "scm-hash.hh"
#include "score.hh"
#include "source.hh"
#include "string.hh"
#include "warn.hh"

My_lily_parser::My_lily_parser (Sources *sources)
{
  book_count_ = 0;
  score_count_ = 0;
  lexer_ = 0;
  sources_ = sources;
  default_duration_ = Duration (2,0);
  error_level_ = 0;
  last_beam_start_ = SCM_EOL;
  header_ = SCM_EOL;

  header_ = ly_make_anonymous_module ();
  smobify_self ();
}

My_lily_parser::~My_lily_parser ()
{
  delete lexer_;
}

IMPLEMENT_SMOBS (My_lily_parser);
IMPLEMENT_TYPE_P (My_lily_parser, "ly:my-lily-parser?");
IMPLEMENT_DEFAULT_EQUAL_P (My_lily_parser);

SCM
My_lily_parser::mark_smob (SCM s)
{
  My_lily_parser *parser = (My_lily_parser*) ly_cdr (s);
  return parser->header_;
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
}

void
My_lily_parser::parse_string (String ly_code)
{
  lexer_ = new My_lily_lexer (sources_);
  lexer_->main_input_name_ = "<string>";
  lexer_->main_input_b_ = true;
  SCM nn = lexer_->lookup_identifier ("pitchnames");
  lexer_->push_note_state (alist_to_hashq (nn));

  set_yydebug (0);
  lexer_->new_input (lexer_->main_input_name_, ly_code, sources_);
  do_yyparse ();
  
  if (!define_spots_.is_empty ())
    {
      define_spots_.top ().warning (_ ("Braces don't match"));
      error_level_ = 1;
    }

  error_level_ = error_level_ | lexer_->error_level_;
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
    the current token. That gets the right result for
    note/duration stuff, but doesn't mess up for errors in the 1st token of the line. 
    
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

/*
  no ! suffix since it doesn't modify 1st argument.
 */
LY_DEFINE (ly_set_point_and_click, "ly:set-point-and-click", 1, 0, 0,
	  (SCM what),
	  "Set the options for Point-and-click source specials output. The\n"
"argument is a symbol.  Possible options are @code{none} (no source specials),\n"
"@code{line} and @code{line-column}")
{
  /*
    UGH.
   */
  SCM val = SCM_BOOL_F;
  if (ly_symbol2scm ("line-column") == what)
    val = scm_c_eval_string ("line-column-location");
  else if (what == ly_symbol2scm ("line"))
    val = scm_c_eval_string ("line-location");

  scm_module_define (global_lily_module, ly_symbol2scm ("point-and-click"), val);
  store_locations_global_b = ly_c_procedure_p (val);
  return SCM_UNSPECIFIED;
}


/* Distill full input file name from command argument.  PATH describes
   file name with added default extension, ".ly" if none.  "-" is
   STDIN.  */
Path
distill_inname (String str)
{
  Path p = split_path (str);
  if (str.is_empty () || str == "-")
    p.base = "-";
  else
    {
      String orig_ext = p.ext;
      char const *extensions[] = {"ly", "", 0};
      for (int i = 0; extensions[i]; i++)
	{
	  p.ext = orig_ext;
	  if (*extensions[i] && !p.ext.is_empty ())
	    p.ext += ".";
	  p.ext += extensions[i];
	  if (!global_path.find (p.to_string ()).is_empty ())
	      break;
	}
      /* Reshuffle extension */
      p = split_path (p.to_string ());
    }
  return p;
}

LY_DEFINE (ly_parse_file, "ly:parse-file",
	   1, 0, 0,
	   (SCM name),
	   "Parse a single @code{.ly} file.  "
	   "Upon failure, throw @code{ly-file-failed} key.")
{
  SCM_ASSERT_TYPE (ly_c_string_p (name), name, SCM_ARG1, __FUNCTION__, "string");
  char const *file = SCM_STRING_CHARS (name);
  
  String infile (file);
  Path inpath = distill_inname (infile);
  
  /* By default, use base name of input file for output file name */
  Path outpath = inpath;
  if (inpath.to_string () != "-")
    outpath.ext = output_format_global;
  
  /* By default, write output to cwd; do not copy directory part
     of input file name */
  outpath.root = "";
  outpath.dir = "";
  
  if (!output_name_global.is_empty ())
    outpath = split_path (output_name_global);
  
  String init;
  if (!init_name_global.is_empty ())
    init = init_name_global;
  else
    init = "init.ly";
  
  String in_file = inpath.to_string ();
  String out_file = outpath.to_string ();


  if (init.length () && global_path.find (init).is_empty ())
    {
      warning (_f ("can't find init file: `%s'", init));
      warning (_f ("(search path: `%s')", global_path.to_string ().to_str0 ()));
      exit (2);
    }

  if ((in_file != "-") && global_path.find (in_file).is_empty ())
    {
      warning (_f ("can't find file: `%s'", in_file));
      scm_throw (ly_symbol2scm ("ly-file-failed"), scm_list_1 (scm_makfrom0str (in_file.to_str0 ())));
    }
  else
    {
      Sources sources;
      sources.set_path (&global_path);
  
      progress_indication (_f ("Now processing `%s'", in_file.to_str0 ()));
      progress_indication ("\n");

      My_lily_parser *parser = new My_lily_parser (&sources);
      parser->parse_file (init, in_file, out_file);

      bool error = parser->error_level_;
      parser = 0;
      if (error)
	/* TODO: pass renamed input file too.  */
	scm_throw (ly_symbol2scm ("ly-file-failed"),
		   scm_list_1 (scm_makfrom0str (in_file.to_str0 ())));
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
  parser->parse_string (ly_scm2string (ly_code));
  parser = 0;
  
  return SCM_UNSPECIFIED;
}


LY_DEFINE (ly_parser_add_book_and_score, "ly:parser-add-book-and-score",
	   2, 0, 0,
	   (SCM purple, SCM music),
	   "Handle the toplevel-music MUSIC by adding BOOK and SCORE.")
{
#if 0
  SCM_ASSERT_TYPE (ly_c_parser_p (parser), music, SCM_ARG1, __FUNCTION__, "parser");
  SCM_ASSERT_TYPE (ly_c_music_p (music), music, SCM_ARG1, __FUNCTION__, "music");
#endif
  My_lily_parser *parser = unsmob_my_lily_parser (purple);
  Score *score = new Score;
  
  /* URG? */
  SCM check_funcs = ly_scheme_function ("toplevel-music-functions");
  for (; ly_c_pair_p (check_funcs); check_funcs = ly_cdr (check_funcs))
    music = scm_call_1 (ly_car (check_funcs), music);
  score->music_ = music;
  Book *book = new Book;
  book->scores_.push (score);
  scm_gc_unprotect_object (score->self_scm ());
  
  SCM header = parser->header_;
  Path outname = split_path (parser->output_basename_);
  int *c = &parser->book_count_;
  if (*c)
    outname.base += "-" + to_string (*c);
  (*c)++;
  My_lily_lexer *lexer = parser->lexer_;
  Music_output_def *dp
    = unsmob_music_output_def (lexer->lookup_identifier ("$defaultpaper"));
  book->process (outname.to_string (),
		 dp ? dp->clone () : new Paper_def, header);
  scm_gc_unprotect_object (book->self_scm ());
  return SCM_UNDEFINED;
}
