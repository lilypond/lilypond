/*
  paper-book.hh -- declare Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005  Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef PAPER_BOOK_HH
#define PAPER_BOOK_HH

#include "parray.hh"
#include "protected-scm.hh"
#include "smobs.hh"

class Output_def;
class Stencil;

/** Paper_book collects headers, systems (Paper_system) and texts, and
    exports them to the output backend, either as systems or as
    completely formatted pages.  */

class Paper_book
{
  DECLARE_SMOBS (Paper_book, );

  SCM systems_;
  SCM pages_;

public:
  SCM header_;
  SCM header_0_;
  SCM scores_;
  Output_def *paper_;
  
  Paper_book ();

  void add_score (SCM);
  SCM systems ();
  SCM pages ();
  Stencil book_title ();
  Stencil score_title (SCM);
  void classic_output (String);
  void output (String);
  void post_processing (SCM, SCM);
};

DECLARE_UNSMOB (Paper_book, paper_book)

#endif /* PAPER_BOOK_HH */
