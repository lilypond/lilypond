/*
  warn.cc -- implement warning and error messages. Needs cleanup.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "debug.hh"
#include "my-lily-lexer.hh"
#include "moment.hh"
#include "time-description.hh"
#include "source-file.hh"
#include "source.hh"
#include "main.hh"
#include "input.hh"

ostream &warnout (cerr);
ostream *mlog (&cerr);



void
error_t (String const & s, Moment const & r)
{
  String t_mom = r.truncated ().str () +  (r - r.truncated ()).str ();
  String e=s+ " (t = " +  t_mom + ")";
  error (e);
}

void
error_t (String const & s, Time_description const &t_tdes)
{
  String e=s+ " (at t=" +  String(t_tdes.bars_i_) + ": " +  (t_tdes.whole_in_measure_).str () + ")\n";
  error (e);
}
