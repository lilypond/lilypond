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

class Paper_book
{
  DECLARE_SMOBS (Paper_book, )

public:
  Array<SCM> headers_;
  Array<SCM> global_headers_;
  Link_array<Paper_def> papers_;
  Array<SCM> scores_;
  Real height_;
  SCM copyright_;
  SCM tagline_;

  Paper_book ();
  void init ();
  Link_array<Page> *pages ();
  SCM lines ();
  SCM scopes (int);
  Stencil* title (int);
  void output (String);
  void classic_output (String);
};

DECLARE_UNSMOB (Paper_book, paper_book)

#endif /* PAPER_BOOK_HH */

