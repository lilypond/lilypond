/*
  minimal-page-breaking.hh -- declare a page-breaker that stacks as
  many systems on a page before moving to the next one. Specialized
  for books with many pages, or a lot of text.

  source file of the GNU LilyPond music typesetter

  (c) 2007--2009 Nicolas Sceaux <nicolas.sceaux@free.fr>
*/

#ifndef MINIMAL_PAGE_BREAKING_HH
#define MINIMAL_PAGE_BREAKING_HH

#include "page-breaking.hh"
#include "page-spacing.hh"

class Minimal_page_breaking: public Page_breaking
{
public:
  virtual SCM solve ();

  Minimal_page_breaking (Paper_book *pb);
  virtual ~Minimal_page_breaking ();
};

#endif /* MINIMAL_PAGE_BREAKING_HH */
