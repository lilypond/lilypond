/*
  request.cc -- implement all musical requests.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "misc.hh"
#include "debug.hh"

#include "music-list.hh"

void
Span_req::do_print () const
{
#ifndef NPRINT
  DOUT << spantype_;
#endif
}

Abbreviation_req::Abbreviation_req ()
{
  type_i_ = 0;
}

void
Abbreviation_req::do_print () const
{
#ifndef NPRINT
  DOUT << "type " << type_i_ << '\n';
#endif
}




void
Melodic_req::transpose (Musical_pitch delta)
{
  pitch_.transpose (delta);
  
  if (abs (pitch_.accidental_i_) > 2)
    {
	warning (_f ("transposition by %s makes accidental larger than two",
	  delta.str ()));
    }
}



bool
Melodic_req::do_equal_b (Request*r) const
{
  Melodic_req* m= dynamic_cast <Melodic_req *> (r);
  return m&& !compare (*m, *this);
}

int
Melodic_req::compare (Melodic_req const &m1 , Melodic_req const&m2)
{
  return Musical_pitch::compare (m1.pitch_, m2.pitch_);
}

void
Melodic_req::do_print () const
{
  pitch_.print ();
}




int
Rhythmic_req::compare (Rhythmic_req const &r1, Rhythmic_req const &r2)
{
  return (r1.length_mom () - r2.length_mom ());
}

bool
Rhythmic_req::do_equal_b (Request*r) const
{
  Rhythmic_req* rh = dynamic_cast <Rhythmic_req *> (r);

  return rh && !compare (*this, *rh);
}

void
Rhythmic_req::do_print () const
{
#ifndef NPRINT
  DOUT << "duration { " <<duration_.str () << "}";
#endif
}


Moment
Rhythmic_req::length_mom () const
{
  return duration_.length_mom ();
}

void
Rhythmic_req::compress (Moment m)
{
  duration_.compress (m);
}

void
Lyric_req::do_print () const
{
  Rhythmic_req::do_print ();
}


bool
Note_req::do_equal_b (Request*r) const
{
  Note_req *n = dynamic_cast<Note_req*> (r);
  return n&& Rhythmic_req::do_equal_b (n) && Melodic_req::do_equal_b (n);
}


Note_req::Note_req ()
{
  cautionary_b_ = false;
  forceacc_b_ = false;
}



void
Note_req::do_print () const
{
#ifndef NPRINT
  Melodic_req::do_print ();
  if (cautionary_b_)
    {
	DOUT << " force cautionary accidental\n";
    }
  else if (forceacc_b_)
    {
	DOUT << " force accidental\n";
    }
  Rhythmic_req::do_print ();
#endif
}


Abbreviation_beam_req::Abbreviation_beam_req ()
{
  type_i_ = 0;
}

bool
Span_req::do_equal_b (Request*r) const
{
  Span_req * s = dynamic_cast <Span_req *> (r);
  return s && spantype_ == s->spantype_;
}

Span_req::Span_req ()
{
  spantype_ = CENTER;
}


void
Absolute_dynamic_req::do_print () const
{
#ifndef NPRINT
  DOUT << " loudness " <<loudness_str_ ;
#endif
}

bool
Absolute_dynamic_req::do_equal_b (Request *r) const
{
  Absolute_dynamic_req *a = dynamic_cast <Absolute_dynamic_req *> (r);
  return a&& loudness_str_ == a->loudness_str_;
}

Absolute_dynamic_req::Absolute_dynamic_req ()
{
  loudness_str_ = "fm";		// yes, "illegal" on purpose.
}



bool
Span_dynamic_req::do_equal_b (Request *req) const
{
  Span_dynamic_req * s = dynamic_cast <Span_dynamic_req *> (req);

  return s&& Span_req::do_equal_b (req) && s->dynamic_dir_ == dynamic_dir_;
}

Span_dynamic_req::Span_dynamic_req ()
{
  dynamic_dir_  = CENTER;
}

void
Span_dynamic_req::do_print () const
{
#ifndef NPRINT
  Span_req::do_print ();
  DOUT << "softer/louder: " << dynamic_dir_;
#endif
}

void
Text_script_req::do_print () const
{
  DOUT << "text" << text_str_
       << ", style = " << style_str_;
}

void
Articulation_req::do_print () const
{
  DOUT << articulation_str_;
}

bool
Articulation_req::do_equal_b (Request*r) const
{
  Articulation_req * a = dynamic_cast<Articulation_req*>(r);
  return articulation_str_ == a->articulation_str_;
}
