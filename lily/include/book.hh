/*
  book.hh -- declare Book

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef BOOK_HH
#define BOOK_HH

#include "input.hh"
#include "lily-proto.hh"
#include "std-vector.hh"
#include "object-key.hh"
#include "std-string.hh"

class Book : public Input
{
  DECLARE_SMOBS (Book, foo);

public:
  std::string user_key_;
  SCM header_;
  Output_def *paper_;
  SCM scores_;

  Book *clone () const;
  Book ();
  void add_score (SCM);
  Paper_book *process (Output_def *def_paper,
		       Output_def *def_layout);
  void set_keys ();
};

DECLARE_UNSMOB (Book, book);

#endif /* BOOK_HH */
