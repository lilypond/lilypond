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

#define PAGE_LAYOUT "ps"

class Paper_book
{
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
  Link_array<Page> *get_pages ();
  Link_array<Page> *fill_pages (Page*, int, Real);
  SCM get_scopes (int);
  Stencil* get_title (int);
  void output (String);
  void classic_output (String);
  DECLARE_SMOBS (Paper_book, )
};

DECLARE_UNSMOB (Paper_book, paper_book)

#endif /* PAPER_BOOK_HH */

