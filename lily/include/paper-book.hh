/*
  paper-book.hh -- declare Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef PAPER_BOOK_HH
#define PAPER_BOOK_HH

// #define PAGE_LAYOUT 1

#include "parray.hh"
class Page;

class Paper_book
{
public:
  Link_array<Paper_score> paper_scores_;
  Paper_book ();

  Link_array<Page> *get_pages ();
  void output ();
};

#endif /* PAPER_BOOK_HH */

