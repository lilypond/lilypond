/*
  command-request.cc -- implement non-musical reqs

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "command-request.hh"
#include "debug.hh"
#include "musical-request.hh"


Bar_req::Bar_req (String s)
{
  set_mus_property ("type", ly_str02scm (s.ch_C()));
}

bool
Barcheck_req::do_equal_b (Request const *r) const
{
  Barcheck_req  const*b = dynamic_cast<Barcheck_req const*> (r);
  return b;
}


Clef_change_req::Clef_change_req ()
{

}


bool
Time_signature_change_req::do_equal_b (Request const *r) const
{
  Time_signature_change_req  const* m
    = dynamic_cast <Time_signature_change_req  const*> (r);

#if 0
  return m && m->beats_i_ == beats_i_
    && one_beat_i_ == m->one_beat_i_;
#endif
  return m;
}

Time_signature_change_req::Time_signature_change_req ()
{
}


Tempo_req::Tempo_req ()
{
  dur_. durlog_i_ = 2;
}



bool
Tempo_req::do_equal_b (Request const *r) const
{
  Tempo_req const *t = dynamic_cast <Tempo_req const*> (r);

  return t&& t->dur_.length_mom ()== dur_.length_mom ();
  // && metronome_i_ == t->metronome_i_;
}




bool
Key_change_req::do_equal_b (Request const * req) const
{
  Key_change_req const * k = dynamic_cast<Key_change_req const*> (req);
  return k && scm_equal_p (get_mus_property ("pitch-alist"), k->get_mus_property ("pitch-alist"));
}


void
Key_change_req::transpose (Musical_pitch p)
{
  SCM newlist = SCM_EOL;
  SCM pa = get_mus_property ("pitch-alist");
  for (SCM s = pa; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM k = gh_caar (s);

      if (gh_pair_p (k))
	{
	  Musical_pitch orig (gh_list (gh_car (k), gh_cdr (k), gh_cdr (s), SCM_UNDEFINED));

	  orig.transpose (p);

	  SCM key = gh_cons (gh_int2scm (orig.octave_i_),
			     gh_int2scm (orig.notename_i_));

	  newlist = gh_cons (gh_cons (key, gh_int2scm (orig.accidental_i_)),
			     newlist);
	}
      else if (gh_number_p (k))
	{
	  Musical_pitch orig (gh_list (gh_int2scm (0), k, gh_cdar (s), SCM_UNDEFINED));
	  orig.transpose (p);

	  SCM key =gh_int2scm (orig.notename_i_);
	  newlist = gh_cons (gh_cons (key, gh_int2scm (orig.accidental_i_)),
			     newlist);
	}
    }

  set_mus_property ("pitch-alist", newlist);
}

Break_req::Break_req ()
{
}


bool
Mark_req::do_equal_b (Request const * r) const
{
  Mark_req const * other = dynamic_cast<Mark_req const*> (r);
  return other && scm_equal_p (other->get_mus_property ("mark-label"),
			       get_mus_property ("mark-label"));
}
