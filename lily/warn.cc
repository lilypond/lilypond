/*
  warn.cc -- implement warning and error messages. Needs cleanup.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
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
  String t_mom = r.trunc_rat ().str () +  (r - r.trunc_rat ()).str ();
  String e=s+ " (t = " +  t_mom + ")";
  error (e);
}

void
error_t (String const & s, Time_description const &t_tdes)
{
  String e=s+ " (at t=" +  to_str (t_tdes.bars_i_) + ": " +  (t_tdes.whole_in_measure_).str () + ")\n";
  error (e);
}
