/*
  event.cc -- implement Event

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "event.hh"
#include "warn.hh"
#include "event.hh"
  
Moment
Event::get_length () const
{
  Duration *d = unsmob_duration (get_property ("duration"));
  if (!d)
    {
      Moment m ;
      return m;
    }
  return d->get_length ();
}

void
Event::compress (Moment m)
{
  Duration *d =  unsmob_duration (get_property ("duration"));
  if (d)
    set_property ("duration", d ->compressed (m.main_part_).smobbed_copy ());
}

void
Event::transpose (Pitch delta)
{
  /*
    TODO: should change music representation such that
    _all_ pitch values are transposed automatically.
   */
  
  Pitch *p = unsmob_pitch (get_property ("pitch"));
  if (!p)
    return ;

  Pitch np = p->transposed (delta);
  
  if (abs (np.get_alteration ()) > DOUBLE_SHARP)
    {
	warning (_f ("Transposition by %s makes alteration larger than two",
	  delta.to_string ()));
    }

  set_property ("pitch", np.smobbed_copy ());
}

Pitch
Event::to_relative_octave (Pitch last)
{
  Pitch *old_pit = unsmob_pitch (get_property ("pitch"));
  if (old_pit)
    {
      Pitch new_pit = *old_pit;
      new_pit = new_pit.to_relative_octave (last);

      SCM check = get_property ("absolute-octave");
      if (is_number (check) &&
	  new_pit.get_octave () != ly_scm2int (check))
	{
	  Pitch expected_pit (ly_scm2int (check),
			      new_pit.get_notename (),
			      new_pit.get_alteration ());
	  origin ()->warning (_f ("octave check failed; expected %s, found: %s",
				  expected_pit.to_string (),
				  new_pit.to_string ()));
	  new_pit = expected_pit;
	}
      
      set_property ("pitch", new_pit.smobbed_copy ());
  
      return new_pit;
    }
  return last;
}
  
Event::Event ()
  : Music ()
{
}

ADD_MUSIC (Event);

LY_DEFINE (ly_music_duration_length, "ly:music-duration-length", 1, 0,0,
	  (SCM mus),
	  "Extract the duration field from @var{mus}, and return the length.")
{
  Music* m =   unsmob_music (mus);
  SCM_ASSERT_TYPE (m, mus, SCM_ARG1, __FUNCTION__, "Music");
  
  Duration *d = unsmob_duration (m->get_property ("duration"));

  Moment l ;
  
  if (d)
    {
      l = d->get_length ();  
    }
  else
    programming_error ("Music has no duration");
  return l.smobbed_copy ();
  
}


LY_DEFINE (ly_music_duration_compress, "ly:music-duration-compress", 2, 0,0,
	  (SCM mus, SCM fact),
	  "Compress @var{mus} by factor @var{fact}, which is a @code{Moment}.")
{
  Music* m =   unsmob_music (mus);
  Moment * f = unsmob_moment (fact);
  SCM_ASSERT_TYPE (m, mus, SCM_ARG1, __FUNCTION__, "Music");
  SCM_ASSERT_TYPE (f, fact, SCM_ARG2, __FUNCTION__, "Moment");
  
  Duration *d = unsmob_duration (m->get_property ("duration"));
  if (d)
    m->set_property ("duration", d->compressed (f->main_part_).smobbed_copy ());
  return SCM_UNSPECIFIED;
}



/*
  This is hairy, since the scale in a key-change event may contain
  octaveless notes.


  TODO: this should use ly:pitch. 
 */
LY_DEFINE (ly_transpose_key_alist, "ly:transpose-key-alist",
	  2, 0, 0, (SCM l, SCM pit),
	  "Make a new key alist of @var{l} transposed by pitch @var{pit}")
{
  SCM newlist = SCM_EOL;
  Pitch *p = unsmob_pitch (pit);
  
  for (SCM s = l; ly_c_pair_p (s); s = ly_cdr (s))
    {
      SCM key = ly_caar (s);
      SCM alter = ly_cdar (s);
      if (ly_c_pair_p (key))
	{
	  Pitch orig (ly_scm2int (ly_car (key)),
		      ly_scm2int (ly_cdr (key)),
		      ly_scm2int (alter));

	  orig =orig.transposed (*p);

	  SCM key = scm_cons (scm_int2num (orig.get_octave ()),
			     scm_int2num (orig.get_notename ()));

	  newlist = scm_cons (scm_cons (key, scm_int2num (orig.get_alteration ())),
			     newlist);
	}
      else if (is_number (key))
	{
	  Pitch orig (0, ly_scm2int (key), ly_scm2int (alter));
	  orig = orig.transposed (*p);

	  key =scm_int2num (orig.get_notename ());
	  alter = scm_int2num (orig.get_alteration ());
	  newlist = scm_cons (scm_cons (key, alter), newlist);
	}
    }
  return scm_reverse_x (newlist, SCM_EOL);
}

void
Key_change_ev::transpose (Pitch p)
{
  SCM pa = get_property ("pitch-alist");

  set_property ("pitch-alist", ly_transpose_key_alist (pa, p.smobbed_copy ()));
  Pitch tonic = *unsmob_pitch (get_property ("tonic"));
  set_property ("tonic",
		    tonic.smobbed_copy ());
}

bool
alist_equal_p (SCM a, SCM b)
{
  for (SCM s = a;
       ly_c_pair_p (s); s = ly_cdr (s))
    {
      SCM key = ly_caar (s);
      SCM val = ly_cdar (s);
      SCM l = scm_assoc (key, b);

      if (l == SCM_BOOL_F
	  || !is_equal ( ly_cdr (l), val))

	return false;
    }
  return true;
}
ADD_MUSIC (Key_change_ev);
