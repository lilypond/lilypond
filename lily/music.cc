/*
  music.cc -- implement Music

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "music.hh"
#include "music-sequence.hh"
#include "duration.hh"
#include "input-smob.hh"
#include "ly-smobs.icc"
#include "main.hh"
#include "pitch.hh"
#include "score.hh"
#include "warn.hh"

/*
  Music is anything that has duration and supports both time compression
  and transposition.

  In Lily, everything that can be thought to have a length and a pitch
  (which has a duration which can be transposed) is considered "music",
*/
bool
Music::internal_is_music_type (SCM k) const
{
  SCM ifs = get_property ("types");

  return scm_c_memq (k, ifs) != SCM_BOOL_F;
}

String
Music::name () const
{
  SCM nm = get_property ("name");
  if (scm_is_symbol (nm))
    return ly_symbol2string (nm);
  else
    return "Music";
}

Music::Music (SCM init)
{
  self_scm_ = SCM_EOL;
  immutable_property_alist_ = init;
  mutable_property_alist_ = SCM_EOL;
  smobify_self ();

  length_callback_ = get_property ("length-callback");
  if (!ly_is_procedure (length_callback_))
    length_callback_ = duration_length_callback_proc;

  start_callback_ = get_property ("start-callback");
}

Music::Music (Music const &m)
{
  immutable_property_alist_ = m.immutable_property_alist_;
  mutable_property_alist_ = SCM_EOL;
  self_scm_ = SCM_EOL;

  /* First we smobify_self, then we copy over the stuff.  If we don't,
     stack vars that hold the copy might be optimized away, meaning
     that they won't be protected from GC. */
  smobify_self ();
  mutable_property_alist_ = ly_music_deep_copy (m.mutable_property_alist_);
  length_callback_ = m.length_callback_;
  start_callback_ = m.start_callback_;
  set_spot (*m.origin ());
}

Music::~Music ()
{
}

SCM
Music::get_property_alist (bool m) const
{
  return (m) ? mutable_property_alist_ : immutable_property_alist_;
}

SCM
Music::mark_smob (SCM m)
{
  Music *mus = (Music *) SCM_CELL_WORD_1 (m);
  scm_gc_mark (mus->immutable_property_alist_);
  return mus->mutable_property_alist_;
}

Moment
Music::get_length () const
{
  SCM lst = get_property ("length");
  if (unsmob_moment (lst))
    return *unsmob_moment (lst);

  if (ly_is_procedure (length_callback_))
    {
      SCM res = scm_call_1 (length_callback_, self_scm ());
      return *unsmob_moment (res);
    }

  return Moment (0);
}

Moment
Music::start_mom () const
{
  SCM lst = start_callback_;
  if (ly_is_procedure (lst))
    {
      SCM res = scm_call_1 (lst, self_scm ());
      return *unsmob_moment (res);
    }

  Moment m;
  return m;
}

void
print_alist (SCM a, SCM port)
{
  /* SCM_EOL  -> catch malformed lists.  */
  for (SCM s = a; scm_is_pair (s); s = scm_cdr (s))
    {
      scm_display (scm_caar (s), port);
      scm_puts (" = ", port);
      scm_write (scm_cdar (s), port);
      scm_puts ("\n", port);
    }
}

int
Music::print_smob (SCM s, SCM p, scm_print_state*)
{
  scm_puts ("#<Music ", p);
  Music *m = unsmob_music (s);

  SCM nm = m->get_property ("name");
  if (scm_is_symbol (nm) || scm_is_string (nm))
    scm_display (nm, p);
  else
    scm_puts ("Music", p);

  /* Printing properties takes a lot of time, especially during backtraces.
     For inspecting, it is better to explicitly use an inspection
     function.  */

  scm_puts (">", p);
  return 1;
}

Pitch
Music::generic_to_relative_octave (Pitch last)
{
  SCM elt = get_property ("element");
  Pitch *old_pit = unsmob_pitch (get_property ("pitch"));
  if (old_pit)
    {
      Pitch new_pit = *old_pit;
      new_pit = new_pit.to_relative_octave (last);

      SCM check = get_property ("absolute-octave");
      if (scm_is_number (check)
	  && new_pit.get_octave () != scm_to_int (check))
	{
	  Pitch expected_pit (scm_to_int (check),
			      new_pit.get_notename (),
			      new_pit.get_alteration ());
	  origin ()->warning (_f ("octave check failed; expected %s, found: %s",
				  expected_pit.to_string (),
				  new_pit.to_string ()));
	  new_pit = expected_pit;
	}

      set_property ("pitch", new_pit.smobbed_copy ());

      last = new_pit;
    }

  if (Music *m = unsmob_music (elt))
    last = m->to_relative_octave (last);

  last = music_list_to_relative (get_property ("elements"), last, false);
  return last;
}

Pitch
Music::to_relative_octave (Pitch last)
{
  SCM callback = get_property ("to-relative-callback");
  if (ly_is_procedure (callback))
    {
      Pitch *p = unsmob_pitch (scm_call_2 (callback, self_scm (), last.smobbed_copy ()));
      return *p;
    }

  return generic_to_relative_octave (last);
}

void
Music::compress (Moment factor)
{
  SCM elt = get_property ("element");

  if (Music *m = unsmob_music (elt))
    m->compress (factor);

  compress_music_list (get_property ("elements"), factor);
  Duration *d = unsmob_duration (get_property ("duration"));
  if (d)
    set_property ("duration", d->compressed (factor.main_part_).smobbed_copy ());
}

void
Music::transpose (Pitch delta)
{
  if (to_boolean (get_property ("untransposable")))
    return;

  for (SCM s = this->get_property_alist (true); scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      SCM val = scm_cdr (entry);

      if (Pitch *p = unsmob_pitch (val))
	{
	  Pitch transposed = p->transposed (delta);
	  scm_set_cdr_x (entry, transposed.smobbed_copy ());

	  if (abs (transposed.get_alteration ()) > DOUBLE_SHARP)
	    {
	      warning (_f ("transposition by %s makes alteration larger than double",
			   delta.to_string ()));
	    }
	}
    }

  SCM elt = get_property ("element");

  if (Music *m = unsmob_music (elt))
    m->transpose (delta);

  transpose_music_list (get_property ("elements"), delta);

  /*
    UGH - how do this more generically?
  */
  SCM pa = get_property ("pitch-alist");
  if (scm_is_pair (pa))
    set_property ("pitch-alist", ly_transpose_key_alist (pa, delta.smobbed_copy ()));
}

IMPLEMENT_TYPE_P (Music, "ly:music?");
IMPLEMENT_SMOBS (Music);
IMPLEMENT_DEFAULT_EQUAL_P (Music);

SCM
Music::internal_get_property (SCM sym) const
{
  SCM s = scm_sloppy_assq (sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return scm_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  return (s == SCM_BOOL_F) ? SCM_EOL : scm_cdr (s);
}

SCM
Music::internal_get_object (SCM s) const
{
  return internal_get_property (s);
}

void
Music::internal_set_object (SCM s, SCM v)
{
  return internal_set_property (s, v);
}

void
Music::internal_set_property (SCM s, SCM v)
{
  if (do_internal_type_checking_global)
    if (!type_check_assignment (s, v, ly_symbol2scm ("music-type?")))
      abort ();

  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, s, v);
}

void
Music::set_spot (Input ip)
{
  set_property ("origin", make_input (ip));
}

Input *
Music::origin () const
{
  Input *ip = unsmob_input (get_property ("origin"));
  return ip ? ip : &dummy_input_global;
}

Music *
make_music_by_name (SCM sym)
{
  SCM make_music_proc = ly_lily_module_constant ("make-music");
  SCM rv = scm_call_1 (make_music_proc, sym);

  /* UGH. */
  Music *m = unsmob_music (rv);
  m->protect ();
  return m;
}

MAKE_SCHEME_CALLBACK (Music, duration_length_callback, 1);
SCM
Music::duration_length_callback (SCM m)
{
  Music *me = unsmob_music (m);
  Duration *d = unsmob_duration (me->get_property ("duration"));

  Moment mom;
  if (d)
    mom = d->get_length ();
  return mom.smobbed_copy ();
}
