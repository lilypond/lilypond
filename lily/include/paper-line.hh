/*
  paper-line.hh -- declare Paper_line

  source file of the GNU LilyPond music typesetter

  (c) 2004  Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef PAPER_LINE_HH
#define PAPER_LINE_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "offset.hh"
#include "stencil.hh"

class Paper_line
{
  DECLARE_SMOBS (Paper_line, );
  Stencil stencil_;
  bool is_title_;
  
public:
  int penalty_;
  int number_;

  Paper_line (Stencil, bool);

  Offset dim () const;
  Stencil to_stencil () const;
  SCM stencils () const;
  bool is_title () const;
  int penalty () const;
};

DECLARE_UNSMOB (Paper_line, paper_line);

#endif /* PAPER_LINE_HH */
