/*
  request.cc -- implement all musical requests.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "misc.hh"
#include "warn.hh"
#include "music-list.hh"


Tremolo_req::Tremolo_req ()
{
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
  Duration *d = unsmob_duration (get_mus_property ("duration"));
  if (!d)
    {
      Moment m ;
      programming_error("Rhythmic_req has no duration");
      return m;
    }
  return d->length_mom ();
}

void
Rhythmic_req::compress (Moment m)
{
  Duration *d =  unsmob_duration (get_mus_property ("duration"));
  if (d)
    set_mus_property ("duration", d ->compressed (m.main_part_).smobbed_copy ());
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
String_number_req::do_equal_b (Request const* r) const
{
  String_number_req const* s  = dynamic_cast<String_number_req const*> (r);
  return s && gh_equal_p (get_mus_property ("string"),
			  s->get_mus_property ("string"));
}


bool
Articulation_req::do_equal_b (Request const* r) const
{
  Articulation_req const* a = dynamic_cast<Articulation_req const*> (r);
  
  return a && gh_equal_p (get_mus_property ("articulation-type"),
			  r->get_mus_property ("articulation-type"))
    && get_direction () == a->get_direction ();
}



