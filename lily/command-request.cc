/*
  command-request.cc -- implement non-musical reqs

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "command-request.hh"
#include "debug.hh"
#include "musical-request.hh"

void
Cadenza_req::do_print () const
{
#ifndef NPRINT
  DEBUG_OUT << (int)on_b_;
#endif
}

bool
Cadenza_req::do_equal_b (Request const *r) const
{
  Cadenza_req const*cad =  dynamic_cast <Cadenza_req const *> (r);
  return cad && cad->on_b_ == on_b_;
}

Cadenza_req::Cadenza_req (bool b)
{
  on_b_ =b;
}



bool
Bar_req::do_equal_b (Request const *r) const
{
  Bar_req  const* b = dynamic_cast <Bar_req const *> (r);
  return b && type_str_ == b->type_str_;
}

void
Bar_req::do_print () const
{
#ifndef NPRINT
  DEBUG_OUT << type_str_;
#endif
}

Bar_req::Bar_req (String s)
{
  type_str_ = s;
}

Partial_measure_req::Partial_measure_req (Moment m)
{
  length_mom_ =m;
}

bool
Partial_measure_req::do_equal_b (Request const* r) const
{
  Partial_measure_req  const*p = dynamic_cast <Partial_measure_req  const*> (r);

  return p&& p->length_mom_ == length_mom_;
}

bool
Barcheck_req::do_equal_b (Request const *r) const
{
  Barcheck_req  const*b = dynamic_cast<Barcheck_req const*> (r);
  return b;
}

void
Clef_change_req::do_print () const
{
#ifndef NPRINT
  DEBUG_OUT << clef_str_ ;
#endif
}

Clef_change_req::Clef_change_req (String s)
{
  clef_str_ = s;
}

void
Partial_measure_req::do_print () const
{
  DEBUG_OUT << length_mom_;
}

void
Time_signature_change_req::do_print () const
{
#ifndef NPRINT
  DEBUG_OUT << beats_i_ << "/" << one_beat_i_;
#endif
}

bool
Time_signature_change_req::do_equal_b (Request const *r) const
{
  Time_signature_change_req  const* m
    = dynamic_cast <Time_signature_change_req  const*> (r);

  return m && m->beats_i_ == beats_i_
    && one_beat_i_ == m->one_beat_i_;
}

Time_signature_change_req::Time_signature_change_req ()
{
  beats_i_ = 0;
  one_beat_i_ =0;
}


Tempo_req::Tempo_req ()
{
  metronome_i_ = 60;
  dur_. durlog_i_ = 2;
}

void
Tempo_req::do_print () const
{
  DEBUG_OUT << dur_.str () << " = " << metronome_i_;
}


bool
Tempo_req::do_equal_b (Request const *r) const
{
  Tempo_req const *t = dynamic_cast <Tempo_req const*> (r);

  return t&& t->dur_.length_mom ()== dur_.length_mom () && metronome_i_ == t->metronome_i_;
}




void
Key_change_req::do_print () const
{
#ifndef NPRINT
  for (int i=0; i < key_.pitch_arr_.size (); i++)
    {
      key_.pitch_arr_[i].print ();
    }
#endif
}

Key_change_req::Key_change_req ()
{
}

Break_req::Break_req ()
{
  penalty_i_ = 0;
}

Mark_req::Mark_req (String s)
{
  str_ = s;
}

void
Mark_req::do_print () const
{
  DEBUG_OUT << str_;
}
void
Key_change_req::transpose (Musical_pitch p)
{
  key_.transpose (p);
}


			   
