/*
  word-wrap.hh -- declare Word_wrap

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef WORD_WRAP_HH
#define WORD_WRAP_HH

#include "break.hh"

/// wordwrap type algorithm: move to next line if current is optimal.
struct Word_wrap : Break_algorithm {
    virtual Array<Column_x_positions> do_solve() const;
};

#endif // WORD_WRAP_HH
