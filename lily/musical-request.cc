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

LY_DEFINE(music_duration_length, "music-duration-length", 1, 0,0,
	  (SCM mus),
	  "Extract the duration field from @var{mus}, and return the length.")
{
  Music* m =   unsmob_music(mus);
  SCM_ASSERT_TYPE(m, mus, SCM_ARG1, __FUNCTION__, "Music");
  
  Duration *d = unsmob_duration (m->get_mus_property ("duration"));

  Moment l ;
  
  if (d)
    {
      l = d->length_mom ();  
    }
  else
    programming_error("Rhythmic_req has no duration");
  return l.smobbed_copy();
  
}


LY_DEFINE(music_duration_compress, "music-duration-compress", 2, 0,0,
	  (SCM mus, SCM factor),
	  "Extract the duration field from @var{mus}, and compress it.")
{
  Music* m =   unsmob_music(mus);
  Moment * f = unsmob_moment (factor);
  SCM_ASSERT_TYPE(m, mus, SCM_ARG1, __FUNCTION__, "Music");
  SCM_ASSERT_TYPE(f, factor, SCM_ARG2, __FUNCTION__, "Moment");
  
  Duration *d = unsmob_duration (m->get_mus_property ("duration"));
  if (d)
    m->set_mus_property ("duration", d->compressed (f->main_part_).smobbed_copy());
  return SCM_UNSPECIFIED;
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

ADD_MUSIC(String_number_req);
