/*
  request.cc -- implement all musical requests.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "misc.hh"
#include "debug.hh"
#include "music-list.hh"


Tremolo_req::Tremolo_req ()
{
}


void
Melodic_req::transpose (Musical_pitch delta)
{
  Musical_pitch p = *unsmob_pitch (get_mus_property ("pitch"));
  
  p.transpose (delta);
  
  if (abs (p.alteration_i_) > 2)
    {
	warning (_f ("Transposition by %s makes accidental larger than two",
	  delta.str ()));
    }

  set_mus_property ("pitch", p.smobbed_copy ());
}

bool
Melodic_req::do_equal_b (Request const* r) const
{
  Melodic_req const* m= dynamic_cast <Melodic_req const*> (r);
  return m; // && !compare (*m, *this);
}

bool
Rhythmic_req::do_equal_b (Request const* r) const
{
  Rhythmic_req const* rh = dynamic_cast <Rhythmic_req const*> (r);

  return rh; // ;  && !compare (*this, *rh);
}



Moment
Rhythmic_req::length_mom () const
{
  return  unsmob_duration (  get_mus_property( "duration"))->length_mom ();

}

void
Rhythmic_req::compress (Moment m)
{
  Duration *d =  unsmob_duration (get_mus_property( "duration"));

  set_mus_property ("duration", d ->compressed (m).smobbed_copy());
}

bool
Note_req::do_equal_b (Request const* r) const
{
  Note_req const* n = dynamic_cast<Note_req const*> (r);
  return n&& Rhythmic_req::do_equal_b (n) && Melodic_req::do_equal_b (n);
}


Note_req::Note_req ()
{
}





bool
Span_req::do_equal_b (Request const*r) const
{
  Span_req const* s = dynamic_cast <Span_req const*> (r);
  return s && get_span_dir () == s->get_span_dir ();
}

Span_req::Span_req ()
{
}


bool
Text_script_req::do_equal_b (Request const* r) const
{
  Text_script_req const* t  = dynamic_cast<Text_script_req const*> (r);
  return t && gh_equal_p (get_mus_property ("text"),
			  t->get_mus_property ("text"));
}


bool
Articulation_req::do_equal_b (Request const* r) const
{
  Articulation_req const* a = dynamic_cast<Articulation_req const*> (r);
  
  return a; //  &&  articulation_str_ == a->articulation_str_;
}


