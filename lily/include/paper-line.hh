/*
  paper-line.hh -- declare Paper_line

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef PAPER_LINE_HH
#define PAPER_LINE_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "offset.hh"

class Paper_line
{
  DECLARE_SMOBS (Paper_line,)
  SCM stencils_;
  Offset dim_;
  bool is_title_;
  
public:
  Paper_line (Offset, SCM, bool = false);

  int number_;
  Offset dim () const;
  SCM stencils () const;
  bool is_title () const;
};

DECLARE_UNSMOB (Paper_line, paper_line);

#endif /* PAPER_LINE_HH */
