/*
  music.cc -- implement Music

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "main.hh"
#include "input-smob.hh"
#include "music.hh"
#include "music-list.hh"
#include "warn.hh"
#include "pitch.hh"
#include "ly-smobs.icc"
#include "main.hh"


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
  if (is_symbol (nm))
    {
      return ly_symbol2string (nm);
    }
  else
    {
      return classname (this);
    }
}

Music::Music ()
{
  self_scm_ = SCM_EOL;
  immutable_property_alist_ = SCM_EOL;
  mutable_property_alist_ = SCM_EOL;
  smobify_self ();
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
  mutable_property_alist_ = ly_deep_mus_copy (m.mutable_property_alist_);
  set_spot (*m.origin ());
}

Music::~Music ()
{
}

ADD_MUSIC (Music);

SCM
Music::get_property_alist (bool m) const
{
  return (m) ? mutable_property_alist_ : immutable_property_alist_;
}

SCM
Music::mark_smob (SCM m)
{
  Music *mus = (Music*) SCM_CELL_WORD_1 (m);
  scm_gc_mark (mus->immutable_property_alist_);
  scm_gc_mark (mus->mutable_property_alist_);
  return SCM_EOL;
}

Moment
Music::get_length () const
{
  SCM lst = get_property ("length");
  if (unsmob_moment (lst))
    return *unsmob_moment (lst);
  else if (is_procedure (lst))
    {
      SCM res = scm_call_1 (lst, self_scm ());
      return *unsmob_moment (res);
    }

  return 0;
}

Moment
Music::start_mom () const
{
  SCM lst = get_property ("start-moment-function");
  if (is_procedure (lst))
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
  for (SCM s = a; ly_c_pair_p (s); s = ly_cdr (s))
    {
      scm_display (ly_caar (s), port);
      scm_puts (" = ", port);
      scm_write (ly_cdar (s), port);
      scm_puts ("\n", port);
    }
}

int
Music::print_smob (SCM s, SCM p, scm_print_state*)
{
  scm_puts ("#<Music ", p);
  Music* m = unsmob_music (s);

  SCM nm = m->get_property ("name");
  if (is_symbol (nm) || ly_c_string_p (nm))
    scm_display (nm, p);
  else
    scm_puts (classname (m),p);

  /* Printing properties takes a lot of time, especially during backtraces.
     For inspecting, it is better to explicitly use an inspection
     function.  */

  scm_puts (">",p);
  return 1;
}

Pitch
Music::to_relative_octave (Pitch p)
{
  SCM elt = get_property ("element");

  if (Music *m = unsmob_music (elt))
    p = m->to_relative_octave (p);

  p = music_list_to_relative (get_property ("elements"), p, false);
  return p;
}

void
Music::compress (Moment factor)
{
  SCM elt = get_property ("element");

  if (Music *m = unsmob_music (elt))
    m->compress (factor);

  compress_music_list (get_property ("elements"), factor);
}

void
Music::transpose (Pitch delta)
{
  SCM elt = get_property ("element");

  if (Music* m = unsmob_music (elt))
    m->transpose (delta);

  transpose_music_list (get_property ("elements"), delta);
}

IMPLEMENT_TYPE_P (Music, "ly:music?");
IMPLEMENT_SMOBS (Music);
IMPLEMENT_DEFAULT_EQUAL_P (Music);

SCM
Music::internal_get_property (SCM sym) const
{
  SCM s = scm_sloppy_assq (sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return ly_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  return (s == SCM_BOOL_F) ? SCM_EOL : ly_cdr (s);
}

void
Music::internal_set_property (SCM s, SCM v)
{
  if (internal_type_checking_global_b)
    if (!type_check_assignment (s, v, ly_symbol2scm ("music-type?")))
      abort ();

  mutable_property_alist_ =  scm_assq_set_x (mutable_property_alist_, s, v);
}

void
Music::set_spot (Input ip)
{
  set_property ("origin", make_input (ip));
}

Input*
Music::origin () const
{
  Input *ip = unsmob_input (get_property ("origin"));
  return ip ? ip : & dummy_input_global;
}

LY_DEFINE (ly_music_length, "ly:music-length",
	   1, 0, 0, (SCM mus),
	  "Get the length of music expression @var{mus}, and return as a @code{Moment} object.")
{
  Music *sc = unsmob_music (mus);
  SCM_ASSERT_TYPE (sc, mus, SCM_ARG1, __FUNCTION__, "music");
  return sc->get_length ().smobbed_copy ();
}

LY_DEFINE (ly_music_property,
	  "ly:music-property", 2, 0, 0, (SCM mus, SCM sym),
	  "Get the property @var{sym} of music expression @var{mus}.\n"
	  "If @var{sym} is undefined, return @code{' ()}.\n" )
{
  Music * sc = unsmob_music (mus);
  SCM_ASSERT_TYPE (sc, mus, SCM_ARG1, __FUNCTION__, "music");
  SCM_ASSERT_TYPE (is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  return sc->internal_get_property (sym);
}

LY_DEFINE (ly_music_set_property, "ly:music-set-property!",
	  3, 0, 0, (SCM mus, SCM sym, SCM val),
	  "Set property @var{sym} in music expression @var{mus} to @var{val}.")
{
  Music * sc = unsmob_music (mus);
  SCM_ASSERT_TYPE (sc, mus, SCM_ARG1, __FUNCTION__, "music");
  SCM_ASSERT_TYPE (is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  bool ok = type_check_assignment (sym, val, ly_symbol2scm ("music-type?"));
  if (ok)
    {
      sc->internal_set_property (sym, val);
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_music_name, "ly:music-name",
	   1, 0, 0, (SCM mus),
	   "Return the name of @var{music}.")
{
  Music *m = unsmob_music (mus);
  SCM_ASSERT_TYPE (m, mus, SCM_ARG1, __FUNCTION__ ,"music");

  char const *nm = classname (m);
  return scm_makfrom0str (nm);
}

/* todo:  property args */
LY_DEFINE (ly_extended_make_music, "ly:make-bare-music",
	   2, 0, 0, (SCM type, SCM props),
	  "Make a C++ music object of type @var{type}, initialize with\n"
	  "@var{props}. \n\n"
	  ""
	  "This function is for internal use, and is only called by "
	  "@code{make-music}, which is the preferred interface "
	  "for creating music objects. "
	  )
{
  SCM_ASSERT_TYPE (ly_c_string_p (type), type, SCM_ARG1, __FUNCTION__, "string");
  SCM s = make_music (ly_scm2string (type))->self_scm ();
  unsmob_music (s)->immutable_property_alist_ = props;
  scm_gc_unprotect_object (s);
  return s;
}

/* todo: property args */
LY_DEFINE (ly_music_mutable_properties, "ly:music-mutable-properties",
	  1, 0, 0, (SCM mus),
	  "Return an alist containing the mutable properties of @var{mus}.\n"
	  "The immutable properties are not available, since "
	  "they are constant and initialized by the "
	   "@code{make-music} function.\n")
{
  Music *m = unsmob_music (mus);
  SCM_ASSERT_TYPE (m, mus, SCM_ARG1, __FUNCTION__, "music");
  return m->get_property_alist (true);
}

LY_DEFINE (ly_music_list_p,"ly:music-list?",
	   1, 0, 0, (SCM lst),
	   "Type predicate: return true if @var{lst} is a list "
	   "of music objects.")
{
  if (scm_list_p (lst) == SCM_BOOL_T)
    while (ly_c_pair_p (lst))
      {
	if (!unsmob_music (ly_car (lst)))
	  return SCM_BOOL_F;
	lst = ly_cdr (lst);
      }

  return SCM_BOOL_T;
}

LY_DEFINE (ly_deep_mus_copy, "ly:music-deep-copy",
	  1, 0, 0, (SCM m),
	  "Copy @var{m} and all sub expressions of @var{m}")
{
  SCM copy = m;
  if (unsmob_music (m))
    {
      copy = unsmob_music (m)->clone ()->self_scm ();
      scm_gc_unprotect_object (copy);
    }
  else if (ly_c_pair_p (m))
    copy = scm_cons (ly_deep_mus_copy (ly_car (m)),
		    ly_deep_mus_copy (ly_cdr (m)));
  return copy;
}

LY_DEFINE (ly_music_transpose, "ly:music-transpose",
	  2, 0, 0, (SCM m, SCM p),
	  "Transpose @var{m} such that central C is mapped to @var{p}. "
	  "Return @var{m}.")
{
  Music * sc = unsmob_music (m);
  Pitch * sp = unsmob_pitch (p);
  SCM_ASSERT_TYPE (sc, m, SCM_ARG1, __FUNCTION__, "music");
  SCM_ASSERT_TYPE (sp, p, SCM_ARG2, __FUNCTION__, "pitch");

  sc->transpose (*sp);
  // SCM_UNDEFINED ?
  return sc->self_scm ();
}

/*
  TODO: should take moment factor?
 */
LY_DEFINE (ly_music_compress, "ly:music-compress",
	  2, 0, 0, (SCM m, SCM factor),
	  "Compress music object @var{m} by moment @var{factor}."
	   )
{
  Music * sc = unsmob_music (m);

  SCM_ASSERT_TYPE (sc, m, SCM_ARG1, __FUNCTION__, "music");
  SCM_ASSERT_TYPE (unsmob_moment (factor), factor, SCM_ARG2, __FUNCTION__, "moment");
  
  sc->compress (*unsmob_moment (factor));
  return sc->self_scm ();
}

Music*
make_music_by_name (SCM sym)
{
  SCM make_music_proc = ly_scheme_function ("make-music");
  SCM rv = scm_call_1 (make_music_proc, sym);

  /* UGH. */
  scm_gc_protect_object (rv);
  return unsmob_music (rv);
}

