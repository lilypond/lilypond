/*
  command-request.cc -- implement non-musical reqs

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "command-request.hh"
#include "debug.hh"
#include "musical-request.hh"


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
}


void
Mark_req::do_print () const
{
  DEBUG_OUT << str_;
}

bool
Mark_req::do_equal_b (Request const * r) const
{
  Mark_req const * other = dynamic_cast<Mark_req const*> (r);
  return other && other->str_ == str_;
}

void
Key_change_req::transpose (Musical_pitch p)
{
  key_.transpose (p);
}


			   
