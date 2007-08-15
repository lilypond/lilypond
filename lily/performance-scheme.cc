/*
  performance-scheme.cc -- implement Performance bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "performance.hh"

LY_DEFINE (ly_performance_write, "ly:performance-write",
	   2, 0, 0, (SCM performance, SCM filename),
	   "Write @var{performance} to @var{filename}")
{
  Performance *perf = dynamic_cast<Performance *> (unsmob_music_output (performance));

  SCM_ASSERT_TYPE (perf, performance, SCM_ARG1, __FUNCTION__, "Performance");
  SCM_ASSERT_TYPE (scm_is_string (filename), filename, SCM_ARG2, __FUNCTION__, "file name");

  perf->write_output (ly_scm2string (filename));
  return SCM_UNSPECIFIED;
}

