/*
  book.hh -- declare Book

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef BOOK_HH
#define BOOK_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "std-string.hh"
#include "virtual-methods.hh"
#include "smobs.hh"

class Book
{
  DECLARE_SMOBS (Book);

public:
  string user_key_;
  SCM header_;
  Output_def *paper_;
  SCM scores_;
  SCM bookparts_;
  SCM input_location_;

  Book (Book const &);
  Input *origin() const;
  VIRTUAL_COPY_CONSTRUCTOR(Book, Book);
  Book ();
  void add_score (SCM);
  void add_bookpart ();
  void add_bookpart (SCM);
  void set_parent (Book *parent);
  bool error_found ();
  Paper_book *process (Output_def *def_paper,
		       Output_def *def_layout);
  Paper_book *process (Output_def *default_paper,
                       Output_def *default_layout,
                       Paper_book *parent_part);
  void set_keys ();
};

DECLARE_UNSMOB (Book, book);

#endif /* BOOK_HH */
