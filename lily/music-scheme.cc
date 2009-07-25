/*
  music-scheme.cc -- implement Music bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "music.hh"

#include "duration.hh"
#include "warn.hh"

LY_DEFINE (ly_music_length, "ly:music-length",
	   1, 0, 0, (SCM mus),
	   "Get the length of music expression @var{mus} and return"
	   " it as a @code{Moment} object.")
{
  LY_ASSERT_TYPE (unsmob_music, mus, 1);
  Music *sc = unsmob_music (mus);
  return sc->get_length ().smobbed_copy ();
}

LY_DEFINE (ly_music_property, "ly:music-property",
	   2, 1, 0, (SCM mus, SCM sym, SCM val),
	   "Return the value for property @var{sym} of music expression"
	   " @var{mus}.  If no value is found, return @var{val} or"
	   " @code{'()} if @var{val} is not specified.")
{
  LY_ASSERT_TYPE (unsmob_music, mus, 1);
  return ly_prob_property (mus, sym, val);
}

LY_DEFINE (ly_music_set_property_x, "ly:music-set-property!",
	   3, 0, 0, (SCM mus, SCM sym, SCM val),
	   "Set property @var{sym} in music expression @var{mus} to"
	   " @var{val}.")
{
  LY_ASSERT_TYPE (unsmob_music, mus, 1);

  return ly_prob_set_property_x (mus, sym, val);
}


/* todo:  property args */
LY_DEFINE (ly_make_music, "ly:make-music",
	   1, 0, 0, (SCM props),
	   "Make a C++ @code{Music} object and initialize it with"
	   " @var{props}.\n"
	   "\n"
	   "This function is for internal use and is only called by"
	   " @code{make-music}, which is the preferred interface"
	   " for creating music objects.")
{
  Music *ms = new Music (props);
  return ms->unprotect ();
}

LY_DEFINE (ly_music_p, "ly:music?",
	   1, 0, 0, (SCM obj),
	   "Is @var{obj} a Music object?")
{
  return scm_from_bool (unsmob_music (obj));
}

/* todo: property args */
LY_DEFINE (ly_music_mutable_properties, "ly:music-mutable-properties",
	   1, 0, 0, (SCM mus),
	   "Return an alist containing the mutable properties of @var{mus}."
	   "  The immutable properties are not available, since they are"
	   " constant and initialized by the @code{make-music} function.")
{
  LY_ASSERT_TYPE (unsmob_music, mus, 1);
  Music *m = unsmob_music (mus);
  return m->get_property_alist (true);
}

LY_DEFINE (ly_music_list_p, "ly:music-list?",
	   1, 0, 0, (SCM lst),
	   "Is @var{lst} a list of music objects?")
{
  if (scm_list_p (lst) == SCM_BOOL_T)
    while (scm_is_pair (lst))
      {
	if (!unsmob_music (scm_car (lst)))
	  return SCM_BOOL_F;
	lst = scm_cdr (lst);
      }

  return SCM_BOOL_T;
}

LY_DEFINE (ly_music_deep_copy, "ly:music-deep-copy",
	   1, 0, 0, (SCM m),
	   "Copy @var{m} and all sub expressions of@tie{}@var{m}.")
{
  SCM copy = m;
  if (unsmob_music (m))
    {
      Music *mcopy = unsmob_music (m)->clone ();
      copy = mcopy->unprotect ();
    }
  else if (scm_is_pair (m))
    copy = scm_cons (ly_music_deep_copy (scm_car (m)),
		     ly_music_deep_copy (scm_cdr (m)));
  return copy;
}

LY_DEFINE (ly_music_transpose, "ly:music-transpose",
	   2, 0, 0, (SCM m, SCM p),
	   "Transpose @var{m} such that central@tie{}C is mapped"
	   " to@tie{}@var{p}.  Return@tie{}@var{m}.")
{
  LY_ASSERT_TYPE (unsmob_music, m, 1);
  LY_ASSERT_SMOB (Pitch, p, 2);

  Music *sc = unsmob_music (m);
  Pitch *sp = unsmob_pitch (p);

  sc->transpose (*sp);
  // SCM_UNDEFINED ?
  return sc->self_scm ();
}

/*
  TODO: should take moment factor?
*/
LY_DEFINE (ly_music_compress, "ly:music-compress",
	   2, 0, 0, (SCM m, SCM factor),
	   "Compress music object@tie{}@var{m} by moment @var{factor}.")
{
  LY_ASSERT_TYPE (unsmob_music, m, 1);
  LY_ASSERT_TYPE (unsmob_moment, factor, 2);

  Music *sc = unsmob_music (m);
  sc->compress (*unsmob_moment (factor));
  return sc->self_scm ();
}

LY_DEFINE (ly_music_duration_length, "ly:music-duration-length", 1, 0, 0,
	   (SCM mus),
	   "Extract the duration field from @var{mus} and return the"
	   " length.")
{
  LY_ASSERT_TYPE (unsmob_music, mus, 1);
  Music *m = unsmob_music (mus);

  Duration *d = unsmob_duration (m->get_property ("duration"));
  Moment len;

  if (d)
    len = d->get_length ();
  else
    programming_error ("music has no duration");
  return len.smobbed_copy ();
}

LY_DEFINE (ly_music_duration_compress, "ly:music-duration-compress", 2, 0, 0,
	   (SCM mus, SCM fact),
	   "Compress @var{mus} by factor @var{fact}, which is a"
	   " @code{Moment}.")
{
  LY_ASSERT_TYPE (unsmob_music, mus, 1);
  LY_ASSERT_SMOB (Moment, fact, 2);
  
  Music *m = unsmob_music (mus);
  Moment *f = unsmob_moment (fact);

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
	   "Make a new key alist of@tie{}@var{l} transposed by"
	   " pitch @var{pit}.")
{
  SCM newlist = SCM_EOL;
  Pitch *p = unsmob_pitch (pit);

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM key = scm_caar (s);
      SCM alter = scm_cdar (s);
      if (scm_is_pair (key))
	{
	  Pitch orig (scm_to_int (scm_car (key)),
		      scm_to_int (scm_cdr (key)),
		      ly_scm2rational (alter));

	  orig = orig.transposed (*p);

	  SCM key = scm_cons (scm_from_int (orig.get_octave ()),
			      scm_from_int (orig.get_notename ()));

	  newlist = scm_cons (scm_cons (key, ly_rational2scm (orig.get_alteration ())),
			      newlist);
	}
      else if (scm_is_number (key))
	{
	  Pitch orig (0, scm_to_int (key), ly_scm2rational (alter));
	  orig = orig.transposed (*p);

	  key = scm_from_int (orig.get_notename ());
	  alter = ly_rational2scm (orig.get_alteration ());
	  newlist = scm_cons (scm_cons (key, alter), newlist);
	}
    }
  return scm_reverse_x (newlist, SCM_EOL);
}

