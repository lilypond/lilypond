/*
  default-actions.cc -- implement default toplevel actions for .ly
  file.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "lily-parser.hh"
#include "lily-lexer.hh"
#include "lilypond-key.hh"
#include "book.hh"
#include "paper-book.hh"
#include "score.hh"
#include "file-name.hh"
#include "output-def.hh"




/* TODO: move this to Scheme?  Why take the parser arg, and all the back
   & forth between scm and c++? */
LY_DEFINE (ly_parser_print_score, "ly:parser-print-score",
	   2, 0, 0,
	   (SCM parser_smob, SCM score_smob),
	   "Print score, i.e., the classic way.")
{
  Lily_parser *parser = unsmob_lily_parser (parser_smob);
  Score *score = unsmob_score (score_smob);

  Object_key *key = new Lilypond_general_key (0, score->user_key_, 0);

  if (score->error_found_)
    return SCM_UNSPECIFIED;

  SCM_ASSERT_TYPE (parser, parser_smob, SCM_ARG1, __FUNCTION__, "parser");
  SCM_ASSERT_TYPE (score, score_smob, SCM_ARG2, __FUNCTION__, "score");

  SCM header = ly_c_module_p (score->header_)
    ? score->header_
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
		       paper, header, os, key->self_scm ());

  if (score->defs_.is_empty ())
    {
      Output_def *layout = get_layout (parser);
      default_rendering (score->get_music (),
			 layout->self_scm (),
			 paper,
			 header, os, key->self_scm ());
      
      scm_gc_unprotect_object (layout->self_scm ());
    }

  scm_gc_unprotect_object (paper);
  scm_gc_unprotect_object (key->self_scm ());
  return SCM_UNSPECIFIED;
}


LY_DEFINE (ly_parser_print_book, "ly:parser-print-book",
	   2, 0, 0, (SCM parser_smob, SCM book_smob),
	   "Print book.")
{
  Lily_parser *parser = unsmob_lily_parser (parser_smob);
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
  Paper_book *pb = book->process (outname.to_string (), layout);

  if (pb)
    {
      pb->output (outname.to_string ());
      scm_gc_unprotect_object (pb->self_scm ());
    }

  scm_gc_unprotect_object (layout->self_scm ());
  return SCM_UNSPECIFIED;
}

