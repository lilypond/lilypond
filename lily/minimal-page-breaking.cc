/*
  minimal-page-breaking.cc -- implement a page-breaker that stacks as
  many systems on a page before moving to the next one. Specialized
  for books with many pages, or a lot of text.

  source file of the GNU LilyPond music typesetter

  (c) 2007 Nicolas Sceaux <nicolas.sceaux@free.fr>
*/

#include "international.hh"
#include "minimal-page-breaking.hh"
#include "output-def.hh"
#include "page-spacing.hh"
#include "paper-book.hh"

static bool
is_break (Grob *g)
{
  (void) g; /* shutup warning */
  return false;
}

Minimal_page_breaking::Minimal_page_breaking (Paper_book *pb)
  : Page_breaking (pb, is_break)
{
}

Minimal_page_breaking::~Minimal_page_breaking ()
{
}

SCM
Minimal_page_breaking::solve ()
{
  vsize end = last_break_position ();

  message ("Computing line breaks...");
  set_to_ideal_line_configuration (0, end);
  break_into_pieces (0, end, current_configuration (0));

  message (_ ("Computing page breaks..."));
  vsize first_page_num = robust_scm2int (book_->paper_->c_variable ("part-first-page-number"), 1);
  Page_spacing_result res = pack_systems_on_least_pages (0, first_page_num);
  SCM lines = systems ();
  return make_pages (res.systems_per_page_, lines);
}
