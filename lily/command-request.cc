/*
  command-request.cc -- implement non-musical reqs

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "command-request.hh"
#include "debug.hh"
#include "musical-request.hh"

bool
Barcheck_req::do_equal_b (Request const *r) const
{
  Barcheck_req  const*b = dynamic_cast<Barcheck_req const*> (r);
  return b;
}

Tempo_req::Tempo_req ()
{
  set_mus_property ("duration", Duration (2,0).smobbed_copy ());
}

void
Key_change_req::transpose (Pitch p)
{
  SCM newlist = SCM_EOL;
  SCM pa = get_mus_property ("pitch-alist");
  for (SCM s = pa; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM key = gh_caar (s);
      SCM alter = gh_cdar (s);
      if (gh_pair_p (key))
	{
	  Pitch orig (gh_scm2int (gh_car (key)),
			      gh_scm2int (gh_cdr (key)),
			      gh_scm2int (alter));

	  orig.transpose (p);

	  SCM key = gh_cons (gh_int2scm (orig.octave_i ()),
			     gh_int2scm (orig.notename_i_));

	  newlist = gh_cons (gh_cons (key, gh_int2scm (orig.alteration_i_)),
			     newlist);
	}
      else if (gh_number_p (key))
	{
	  Pitch orig (0, gh_scm2int (key), gh_scm2int (alter));
	  orig.transpose (p);

	  key =gh_int2scm (orig.notename_i_);
	  alter = gh_int2scm (orig.alteration_i_);
	  newlist = gh_cons (gh_cons (key, alter), newlist);
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
			       get_mus_property ("mark-label")) == SCM_BOOL_T;
}


ADD_MUSIC (Articulation_req);
ADD_MUSIC (Barcheck_req);
ADD_MUSIC (Break_req);
ADD_MUSIC (Breathing_sign_req);
ADD_MUSIC (Busy_playing_req);
ADD_MUSIC (Extender_req);
ADD_MUSIC (Glissando_req);
ADD_MUSIC (Hyphen_req);
ADD_MUSIC (Key_change_req);
ADD_MUSIC (Lyric_req);
ADD_MUSIC (Mark_req);
ADD_MUSIC (Melisma_playing_req);
ADD_MUSIC (Melisma_req);
ADD_MUSIC (Melodic_req);
ADD_MUSIC (Note_req);
ADD_MUSIC (Rest_req);
ADD_MUSIC (Rhythmic_req);
ADD_MUSIC (Script_req);
ADD_MUSIC (Skip_req);
ADD_MUSIC (Span_req);
ADD_MUSIC (Tempo_req);
ADD_MUSIC (Text_script_req);
ADD_MUSIC (Tie_req);
ADD_MUSIC (Tremolo_req);
