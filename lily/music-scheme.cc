/*
  music-scheme.cc --  implement

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "music.hh"
#include "pitch.hh"

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
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  return sc->internal_get_property (sym);
}

LY_DEFINE (ly_music_set_property, "ly:music-set-property!",
	  3, 0, 0, (SCM mus, SCM sym, SCM val),
	  "Set property @var{sym} in music expression @var{mus} to @var{val}.")
{
  Music * sc = unsmob_music (mus);
  SCM_ASSERT_TYPE (sc, mus, SCM_ARG1, __FUNCTION__, "music");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

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
  SCM_ASSERT_TYPE (scm_is_string (type), type, SCM_ARG1, __FUNCTION__, "string");
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
	  "Copy @var{m} and all sub expressions of @var{m}")
{
  SCM copy = m;
  if (unsmob_music (m))
    {
      copy = unsmob_music (m)->clone ()->self_scm ();
      scm_gc_unprotect_object (copy);
    }
  else if (scm_is_pair (m))
    copy = scm_cons (ly_music_deep_copy (scm_car (m)),
		    ly_music_deep_copy (scm_cdr (m)));
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

