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

struct Score_lines
{
  SCM lines_;
  SCM header_;

  Score_lines () ;
  void gc_mark ();
};

/*
  
  DOCME.
  
*/
class Paper_book
{
  DECLARE_SMOBS (Paper_book, );

  SCM lines_;
  SCM pages_;

  
  Real height_;			// what is this variable for? 
  SCM copyright_;
  SCM tagline_;
public:
  SCM header_;
  Array<Score_lines> score_lines_;
  Output_def *bookpaper_;
  
  Paper_book ();

  SCM lines ();
  SCM pages ();
  Stencil title (int);
  void classic_output (String);
  void init ();
  void output (String);
};

DECLARE_UNSMOB (Paper_book, paper_book)

#endif /* PAPER_BOOK_HH */

