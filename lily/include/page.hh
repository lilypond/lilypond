/*
  page.hh -- declare Page

  source file of the GNU LilyPond music typesetter

  (c) 2004  Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef PAGE_HH
#define PAGE_HH

#include "lily-proto.hh"
#include "smobs.hh"

/* WIP -- moving toward flexible stencil based output.
   Rename to Paper_page? */
class Page
{
  DECLARE_SMOBS (Page, );

public:
  static int page_count_;
  static Real MIN_COVERAGE_;
  Paper_def *paper_;
  int number_;
  int line_count_;
  SCM lines_;
  SCM header_;
  SCM footer_;
  SCM copyright_;
  SCM tagline_;

  /* actual height filled with text.  */
  Real height_;
  
  // HMMM all this size stuff to paper/paper-outputter?
  Real vsize_;
  Real top_margin_;
  Real bottom_margin_;

  Page (Paper_def*, int);

  /* available area for text.  */
  Real text_height () const;
  Real left_margin () const;
  Stencil to_stencil () const;
};

DECLARE_UNSMOB (Page, page);

#endif /* PAGE_HH */
