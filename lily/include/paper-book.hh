/*
  paper-book.hh -- declare Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004  Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef PAPER_BOOK_HH
#define PAPER_BOOK_HH

#include "lily-guile.hh"
#include "parray.hh"
#include "protected-scm.hh"
#include "smobs.hh"

struct Score_systems
{
  SCM systems_;
  SCM header_;

  Score_systems () ;
  void gc_mark ();
};

/*
  
This class is rather empty. It collects systems (Paper_system), and
exports them to the output backend, either as systems or as completely
formatted pages.
  
*/
class Paper_book
{
  DECLARE_SMOBS (Paper_book, );

  SCM systems_;
  SCM pages_;
public:
  SCM header_;
  Array<Score_systems> score_systems_;
  Output_def *bookpaper_;
  
  Paper_book ();

  SCM systems ();
  SCM pages ();
  Stencil book_title ();
  Stencil score_title (int);
  void classic_output (String);
  void output (String);
  void post_processing (SCM, SCM);
};

DECLARE_UNSMOB (Paper_book, paper_book)

#endif /* PAPER_BOOK_HH */

