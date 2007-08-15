/*
  lily-parser-scheme.cc -- implement Lily_parser bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include <unistd.h>

#include "file-name-map.hh"
#include "file-name.hh"
#include "file-path.hh"
#include "input.hh"
#include "international.hh"
#include "lily-lexer.hh"
#include "lily-parser.hh"
#include "ly-module.hh"
#include "main.hh"
#include "program-option.hh"
#include "source.hh"
#include "warn.hh"

/* Do not append `!' suffix, since 1st argument is not modified. */
LY_DEFINE (ly_set_point_and_click, "ly:set-point-and-click",
	   1, 0, 0, (SCM what),
	   "Deprecated.")
{
  (void) what;
  warning (_f ("deprecated function called: %s", "ly:set-point-and-click"));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_parse_file, "ly:parse-file",
	   1, 0, 0, (SCM name),
	   "Parse a single @code{.ly} file.  "
	   "Upon failure, throw @code{ly-file-failed} key.")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");
  string file = ly_scm2string (name);
  char const *extensions[] = {"ly", "", 0};

  string file_name = global_path.find (file, extensions);

  /* By default, use base name of input file for output file name,
     write output to cwd; do not use root and directory parts of input
     file name.  */
  File_name out_file_name (file_name);

  global_path.append (out_file_name.dir_);

  out_file_name.ext_ = "";
  out_file_name.root_ = "";
  out_file_name.dir_ = "";

  /* When running from gui, generate output in .ly source directory.  */
  if (output_name_global.empty ()
      && ly_get_option (ly_symbol2scm ("gui")) == SCM_BOOL_T)
    {
      File_name f (file);
      f.base_ = "";
      f.ext_ = "";
      output_name_global = f.to_string ();
    }

  if (!output_name_global.empty ())
    {
      
      /* Interpret --output=DIR to mean --output=DIR/BASE.  */
      string dir;
      if (is_dir (output_name_global))
	{
	  dir = output_name_global;
	  output_name_global = "";
	}
      else
	{
	  File_name out (output_name_global);
	  if (is_dir (out.dir_part ()))
	    {
	      dir = out.dir_part ();
	      out_file_name = out.file_part ();
	    }
	}	  

      if (dir != "" && dir != "." && dir != get_working_directory ())
	{
	  global_path.prepend (get_working_directory ());
	  message (_f ("Changing working directory to `%s'",
		       dir.c_str ()));
	  chdir (dir.c_str ());
	}
      else
	out_file_name = File_name (output_name_global);
    }

  string init;
  if (!init_name_global.empty ())
    init = init_name_global;
  else
    init = "init.ly";

  string out_file = out_file_name.to_string ();

  if (init.length () && global_path.find (init).empty ())
    {
      warning (_f ("can't find init file: `%s'", init));
      warning (_f ("(search path: `%s')",
		   global_path.to_string ().c_str ()));
      exit (2);
    }


  bool error = false;
  if ((file_name != "-") && file_name.empty ())
    {
      warning (_f ("can't find file: `%s'", file));
      error = true;
    }
  else
    {
      Sources sources;
      sources.set_path (&global_path);

      string mapped_fn = map_file_name (file_name);
      message (_f ("Processing `%s'", mapped_fn.c_str ()));
      progress_indication ("\n");

      Lily_parser *parser = new Lily_parser (&sources);

      parser->parse_file (init, file_name, out_file);

      error = parser->error_level_;

      parser->clear ();
      parser->unprotect ();
    }

  /*
    outside the if-else to ensure cleanup fo Sources object, 
   */
  if (error)
    /* TODO: pass renamed input file too.  */
    scm_throw (ly_symbol2scm ("ly-file-failed"),
	       scm_list_1 (scm_makfrom0str (file_name.c_str ())));
  
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
  parser->parse_string (ly_scm2string (ly_code));
  parser->unprotect ();
  parser = 0;

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_clone_parser, "ly:clone-parser",
	   1, 0, 0, (SCM parser_smob),
	   "Return a clone of PARSER_SMOB.")
{
  Lily_parser *parser = unsmob_lily_parser (parser_smob);
  Lily_parser *clone = new Lily_parser (*parser);

  return clone->unprotect ();
}

LY_DEFINE (ly_parser_define, "ly:parser-define!",
	   3, 0, 0, (SCM parser_smob, SCM symbol, SCM val),
	   "Bind SYMBOL to VAL in PARSER_SMOB's module.")
{
  Lily_parser *parser = unsmob_lily_parser (parser_smob);
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
  Lily_parser *parser = unsmob_lily_parser (parser_smob);

  SCM_ASSERT_TYPE (scm_is_symbol (symbol), symbol, SCM_ARG2, __FUNCTION__, "symbol");
  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG2, __FUNCTION__, "parser");

  SCM val = parser->lexer_->lookup_identifier (ly_scm2string (scm_symbol_to_string (symbol)));
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
  Lily_parser *parser = unsmob_lily_parser (parser_smob);

  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG1, __FUNCTION__, "parser");
  SCM_ASSERT_TYPE (scm_is_string (ly_code), ly_code, SCM_ARG2, __FUNCTION__, "string");

  parser->parse_string (ly_scm2string (ly_code));

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_parser_set_note_names, "ly:parser-set-note-names",
	   2, 0, 0, (SCM parser, SCM names),
	   "Replace current note names in @var{parser}. "
	   "@var{names} is an alist of symbols.  "
	   "This only has effect if the current mode is notes.")
{
  Lily_parser *p = unsmob_lily_parser (parser);
  SCM_ASSERT_TYPE (p, parser, SCM_ARG1, __FUNCTION__, "Lilypond parser");

  if (p->lexer_->is_note_state ())
    {
      p->lexer_->pop_state ();
      p->lexer_->push_note_state (alist_to_hashq (names));
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_parser_output_name, "ly:parser-output-name",
	   1, 0, 0, (SCM parser),
	   "Return the base name of the output file.")
{
  Lily_parser *p = unsmob_lily_parser (parser);
  SCM_ASSERT_TYPE (p, parser, SCM_ARG1, __FUNCTION__, "Lilypond parser");

  return scm_makfrom0str (p->output_basename_.c_str ());
}

LY_DEFINE (ly_parser_error, "ly:parser-error",
	   2, 1, 0, (SCM parser, SCM msg, SCM input),
	   "Display an error message, and make the parser fail")
{
  Lily_parser *p = unsmob_lily_parser (parser);
  SCM_ASSERT_TYPE (p, parser, SCM_ARG1, __FUNCTION__, "Lilypond parser");
  SCM_ASSERT_TYPE (scm_is_string (msg), msg, SCM_ARG2, __FUNCTION__, "string");
  string s = ly_scm2string (msg);
  
  Input *i = unsmob_input (input);
  if (i)
    p->parser_error (*i, s);
  else
    p->parser_error (s);

  return parser;
}
