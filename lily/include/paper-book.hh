/*
  paper-book.hh -- declare Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PAPER_BOOK_HH
#define PAPER_BOOK_HH

// #define PAGE_LAYOUT 1

#include "lily-guile.hh"
#include "parray.hh"
#include "protected-scm.hh"

class Paper_book
{
public:
  Array<SCM> headers_;
  Link_array<Paper_def> papers_;
  Array<SCM> scores_;

  Paper_book ();
  Link_array<Page> *get_pages ();
  SCM get_scopes (int);
  Stencil* get_title (int);
  void output (String);
  void classic_output (String);
};

#endif /* PAPER_BOOK_HH */

