/*
  book.hh -- declare Book

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef BOOK_HH
#define BOOK_HH

#include "input.hh"
#include "lily-proto.hh"
#include "parray.hh"
#include "smobs.hh"

class Book : public Input
{
  DECLARE_SMOBS (Book, foo);

public:
  SCM header_;
  Output_def *paper_;
  
  Link_array<Score> scores_;
  Book ();

  Paper_book* process (String, Output_def*);
};
DECLARE_UNSMOB (Book, book);

#endif /* BOOK_HH */
