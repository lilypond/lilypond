/*
  performance-scheme.cc -- implement Performance bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "performance.hh"

LY_DEFINE (ly_performance_write, "ly:performance-write",
	   2, 0, 0, (SCM performance, SCM filename),
	   "Write @var{performance} to @var{filename}.")
{
  LY_ASSERT_TYPE (unsmob_performance, performance, 1);
  LY_ASSERT_TYPE (scm_is_string, filename, 2);

  unsmob_performance (performance)->write_output (ly_scm2string (filename));
  return SCM_UNSPECIFIED;
}

