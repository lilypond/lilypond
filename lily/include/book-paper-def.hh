/*
  book-paper-def.hh -- declare Book_output_def

  source file of the GNU LilyPond music typesetter

  (c) 2004  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef BOOK_PAPER_DEF_HH
#define BOOK_PAPER_DEF_HH

#include "lily-proto.hh"
#include "output-def.hh"

class Book_output_def : public Output_def
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Output_def, Book_output_def);
  Book_output_def (Book_output_def const &);
  SCM scaled_fonts_;
  Real output_scale_;
  Real output_scale () const;

  Book_output_def ();

  virtual void derived_mark ();
  Font_metric *find_scaled_font (Font_metric *f, Real m, SCM input_enc_name);
  Output_def *scale_paper (Output_def *pd) const;
};

Book_output_def *unsmob_book_output_def (SCM bp);

#endif /* BOOK_PAPER_DEF_HH */
