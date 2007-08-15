/*
  book-paper-def.hh -- declare Output_def

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef BOOK_PAPER_DEF_HH
#define BOOK_PAPER_DEF_HH

#include "output-def.hh"

#error

class Output_def : public Output_def
{
public:
  VIRTUAL_COPY_CONSTRUCTOR (Output_def, Output_def);
  Output_def (Output_def const &);
  Output_def ();

  virtual void derived_mark ();
};

#endif /* BOOK_PAPER_DEF_HH */
