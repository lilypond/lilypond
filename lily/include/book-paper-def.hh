/*
  book-paper-def.hh -- declare Book_paper_def

  source file of the GNU LilyPond music typesetter

  (c) 2004  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef BOOK_PAPER_DEF_HH
#define BOOK_PAPER_DEF_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "virtual-methods.hh"

class Book_paper_def
{
  DECLARE_SMOBS (Book_paper_def, Music_output_def);

public:
  VIRTUAL_COPY_CONSTRUCTOR (Book_paper_def, Book_paper_def);
  Book_paper_def(Book_paper_def const &);
  SCM scope_;
  SCM scaled_fonts_;
  Real output_scale_;

  Book_paper_def ();

  Font_metric *find_scaled_font (Font_metric *f, Real m, SCM input_enc_name);
  Paper_def *scale_paper (Paper_def *pd) const;
};
DECLARE_UNSMOB (Book_paper_def, book_paper_def);

#endif /* BOOK_PAPER_DEF_HH */
