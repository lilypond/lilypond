/*
  page.hh -- declare Page

  source file of the GNU LilyPond music typesetter

  (c) 2004  Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef PAGE_HH
#define PAGE_HH

#include "lily-proto.hh"
#include "smobs.hh"

/* WIP -- moving toward flexible stencil based output.  */
class Page
{
  DECLARE_SMOBS (Page, );

public:
  static int page_count_;
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
  Real hsize_;
  Real vsize_;
  Real left_margin_;
  Real top_margin_;
  Real bottom_margin_;
  Real foot_sep_;
  Real head_sep_;
  Real text_width_;

  /* available area for text.  */
  Real text_height ();

  Page (Paper_def*, int);
  void output (Paper_outputter*, bool);
};

DECLARE_UNSMOB (Page, page);

#endif /* PAGE_HH */
