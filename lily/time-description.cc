/*
  time-description.cc -- implement Time_description

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "time-description.hh"
#include "debug.hh"

String
Time_description::str () const
{
  String s ("Time_description { ");
  if (cadenza_b_)
    s += String ("(") + _("cadenza") + ")";
  s += "at ";
  s += when_.str ();
  s +="\ntime_signature " + (whole_per_measure_/one_beat_).str () +":" +
    (Moment (Moment (1)/one_beat_)).str ();
  s += "\nposition " + to_str (bars_i_) + ":"+ whole_in_measure_.str () +"\n}\n";
  return s;
}

void
Time_description::print() const
{
#ifndef NPRINT
  DEBUG_OUT << str ();
#endif
}

void
Time_description::OK() const
{
#ifndef NDEBUG
  if (!cadenza_b_)
    assert (whole_in_measure_ < whole_per_measure_);
  assert (Moment (0) <= whole_in_measure_);
  assert (one_beat_);
#endif
}

void
Time_description::set_cadenza (bool b)
{
  if (cadenza_b_ && !b)
    {
      if (whole_in_measure_)
	{
	  bars_i_ ++;		// should do?
	  whole_in_measure_ = 0;
	}
    }
  cadenza_b_ = b ;
}

Time_description::Time_description()
{
  whole_per_measure_ = 1;
  whole_in_measure_ =0;
  one_beat_ = Moment (1,4);
  when_ = 0;
  bars_i_ = 1;			// musician start counting at 1
  cadenza_b_ = false;
}

void
Time_description::add (Moment dt)
{
  assert (dt >= Moment (0));
  when_ +=  dt;
  whole_in_measure_ += dt;

  while (!cadenza_b_ && whole_in_measure_ >= whole_per_measure_)
    {
      whole_in_measure_ -= whole_per_measure_;
      bars_i_ ++;
    }
}

void
Time_description::set_time_signature (int l, int o)
{
  assert (o);
  one_beat_ = Moment (1)/Moment (o);
  whole_per_measure_ = Moment (l) * one_beat_;
}

bool
Time_description::allow_time_signature_change_b()
{
  return!(whole_in_measure_);
}

/**
  retrieve error messages.
  @return
  error messages if not possible, "" if possible
  */
String
Time_description::try_set_partial_str (Moment p) const
{
  if (p<Moment (0))
    return (_ ("Partial measure must be non-negative"));
  if (p > whole_per_measure_)
    return (_ ("partial measure too large"));
  return "";
}

void
Time_description::setpartial (Moment p)
{
  whole_in_measure_ = whole_per_measure_ - p;
}

Moment
Time_description::barleft() const
{
  assert (!cadenza_b_);
  return whole_per_measure_-whole_in_measure_;
}

Moment
Time_description::next_bar_moment() const
{
  return when_ + barleft();
}
