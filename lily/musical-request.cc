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
  DOUT << span_dir_;
#endif
}

Tremolo_req::Tremolo_req ()
{
  type_i_ = 0;
}

void
Tremolo_req::do_print () const
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
#ifndef NPRINT
  Rhythmic_req::do_print ();
  DOUT <<  "text = " << text_str_;
#endif
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


bool
Span_req::do_equal_b (Request*r) const
{
  Span_req * s = dynamic_cast <Span_req *> (r);
  return s && span_dir_ == s->span_dir_;
}

Span_req::Span_req ()
{
  span_dir_ = CENTER;
}


void
Text_script_req::do_print () const
{
  DOUT << "text" << text_str_
       << ", style = " << style_str_;
}

bool
Text_script_req::do_equal_b (Request *r) const
{
  Text_script_req * t  = dynamic_cast<Text_script_req*>(r);
  return t && t->text_str_ == text_str_ && t->style_str_ == style_str_;
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
  
  return a &&  articulation_str_ == a->articulation_str_;
}
