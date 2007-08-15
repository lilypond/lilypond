/*
  paper-book.hh -- declare Paper_book

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007  Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef PAPER_BOOK_HH
#define PAPER_BOOK_HH

#include "std-vector.hh"
#include "smobs.hh"
#include "lily-proto.hh"

/** Paper_book collects headers, systems (Paper_system) and texts, and
    exports them to the output backend, either as systems or as
    completely formatted pages.  */

class Paper_book
{
  DECLARE_SMOBS (Paper_book);

  SCM systems_;
  SCM pages_;
  SCM performances_;

  void add_score_title (SCM);
  SCM get_score_title (SCM);
  
public:
  SCM header_;
  SCM header_0_;
  SCM scores_;
  Output_def *paper_;

  Paper_book ();

  void add_score (SCM);
  void add_performance (SCM);

  SCM performances () const;
  SCM systems ();
  SCM pages ();
  SCM get_system_specs ();


  Stencil book_title ();
  Stencil score_title (SCM);
  
  void classic_output (SCM output_channel);
  void output (SCM output_channel);
};

DECLARE_UNSMOB (Paper_book, paper_book)

#endif /* PAPER_BOOK_HH */
