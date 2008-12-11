/*
  page-marker.cc -- implement Page_marker

  source file of the GNU LilyPond music typesetter

  (c) 2007--2008 Nicolas Sceaux <nicolas.sceaux@free.fr>
*/

#include "page-marker.hh"
#include "ly-smobs.icc"

Page_marker::Page_marker ()
{
  symbol_ = SCM_EOL;
  permission_ = SCM_EOL;
  label_ = SCM_EOL;
  smobify_self ();
}

Page_marker::~Page_marker ()
{
}

IMPLEMENT_SMOBS (Page_marker);
IMPLEMENT_DEFAULT_EQUAL_P (Page_marker);
IMPLEMENT_TYPE_P (Page_marker, "ly:page-marker?");

SCM
Page_marker::mark_smob (SCM smob)
{
  Page_marker *pm = (Page_marker *) SCM_CELL_WORD_1 (smob);
  scm_gc_mark (pm->symbol_);
  scm_gc_mark (pm->permission_);
  scm_gc_mark (pm->label_);
  return SCM_EOL;
}

int
Page_marker::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Page_marker *pm = (Page_marker *) SCM_CELL_WORD_1 (smob);
  (void)pm;
  scm_puts ("#<Page_marker>", port);
  return 1;
}

SCM
Page_marker::permission_symbol ()
{
  return symbol_;
}

SCM
Page_marker::permission_value ()
{
  return permission_;
}

SCM
Page_marker::label ()
{
  return label_;
}

void
Page_marker::set_permission (SCM symbol, SCM permission)
{
  symbol_ = symbol;
  permission_ = permission;
}

void
Page_marker::set_label (SCM label)
{
  label_ = label;
}


