/*
  music.cc -- implement Music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "input-smob.hh"
#include "music.hh"
#include "music-list.hh"
#include "debug.hh"
#include "pitch.hh"
#include "ly-smobs.icc"

SCM
ly_deep_mus_copy (SCM m)
{
  if (unsmob_music (m))
    {
      SCM ss =  unsmob_music (m)->clone ()->self_scm ();
      scm_unprotect_object (ss);
      return ss;
    }
  else if (gh_pair_p (m))
    {
      return gh_cons (ly_deep_mus_copy (gh_car (m)), ly_deep_mus_copy (gh_cdr (m)));
    }
  else
    return m;
}


Music::Music (Music const &m)
{
  immutable_property_alist_ = m.immutable_property_alist_;
  SCM c =ly_deep_mus_copy (m.mutable_property_alist_);
  mutable_property_alist_ = c;

  smobify_self ();

  set_spot (*m.origin ());
}


Music::Music(SCM l)
{
  immutable_property_alist_ = l;
  mutable_property_alist_ = SCM_EOL;
  smobify_self ();
}

SCM
Music::mark_smob (SCM m)
{
  Music * mus = (Music *)SCM_CELL_WORD_1(m);
  scm_gc_mark (mus->immutable_property_alist_);
  scm_gc_mark (mus->mutable_property_alist_);
  return SCM_EOL;
}

void    
Music::compress (Moment)
{
}



Moment
Music::length_mom () const
{
  SCM l = get_mus_property ("length");
  if (unsmob_moment (l))
    return *unsmob_moment(l);
  else if (gh_procedure_p (l))
    {
      SCM res = gh_call1(l, self_scm( ));
      return *unsmob_moment(res);
    }
    
  return 0;
}

void
print_alist (SCM a, SCM port)
{
  for (SCM s = a; gh_pair_p (s); s = gh_cdr (s))
    {
      scm_display (gh_caar (s), port);
      scm_puts (" = ", port); 
      scm_write (gh_cdar (s), port);
      scm_puts ("\n", port);
    }
}

int
Music::print_smob(SCM s, SCM p, scm_print_state*)
{
  scm_puts ("#<Music ", p);
  Music* m = unsmob_music (s);
  scm_puts (classname(m),p);

  print_alist (m->mutable_property_alist_, p);
  print_alist (m->immutable_property_alist_, p);
  
  scm_puts (">",p);
  return 1;
}

Pitch
Music::to_relative_octave (Pitch m)
{
  return m;
}


void
Music::transpose (Pitch )
{
}

IMPLEMENT_TYPE_P(Music, "music?");
IMPLEMENT_UNSMOB(Music,music);
IMPLEMENT_SMOBS(Music);
IMPLEMENT_DEFAULT_EQUAL_P(Music);

/****************************/

SCM
Music::get_mus_property (const char *nm) const
{
  SCM sym = ly_symbol2scm (nm);
  return get_mus_property (sym);
}

SCM
Music::get_mus_property (SCM sym) const
{
  SCM s = scm_sloppy_assq(sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return gh_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  return (s == SCM_BOOL_F) ? SCM_EOL : gh_cdr (s); 
}

/*
  Remove the value associated with KEY, and return it. The result is
  that a next call will yield SCM_EOL (and not the underlying
  `basic' property.
*/
SCM
Music::remove_mus_property (const char* key)
{
  SCM val = get_mus_property (key);
  if (val != SCM_EOL)
    set_mus_property (key, SCM_EOL);
  return val;
}

void
Music::set_mus_property (const char* k, SCM v)
{
  SCM s = ly_symbol2scm (k);
  set_mus_property (s, v);
}

void
Music::set_immutable_mus_property (const char*k, SCM v)
{
  SCM s = ly_symbol2scm (k);
  set_immutable_mus_property (s, v);
}

void
Music::set_immutable_mus_property (SCM s, SCM v)
{
  immutable_property_alist_ = gh_cons (gh_cons (s,v), mutable_property_alist_);
  mutable_property_alist_ = scm_assq_remove_x (mutable_property_alist_, s);
}
void
Music::set_mus_property (SCM s, SCM v)
{
  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, s, v);
}

void
Music::set_spot (Input ip)
{
   set_mus_property ("origin", make_input (ip));
}



Input*
Music::origin () const
{
  Input *ip = unsmob_input (get_mus_property ("origin"));
  return ip ? ip : & dummy_input_global; 
}





Music::~Music ()
{
  
}

SCM
ly_get_mus_property (SCM mus, SCM sym)
{
  Music * sc = unsmob_music (mus);
  
  if (sc)
    {
      return sc->get_mus_property (sym);
    }
  else
    {
      warning (_("ly_get_mus_property (): Not a Music"));
      scm_write (mus, scm_current_error_port ());
    }
  return SCM_EOL;
}


SCM
ly_set_mus_property (SCM mus, SCM sym, SCM val)
{
  Music * sc = unsmob_music (mus);

  if (!gh_symbol_p (sym))
    {
      warning (_("ly_set_mus_property (): Not a symbol"));
      scm_write (mus, scm_current_error_port ());      

      return SCM_UNSPECIFIED;
    }

  if (sc)
    {
      sc->set_mus_property (sym, val);
    }
  else
    {
      warning (_("ly_set_mus_property ():  not of type Music"));
      scm_write (mus, scm_current_error_port ());
    }

  return SCM_UNSPECIFIED;
}

static void
init_functions ()
{
  scm_make_gsubr ("ly-get-mus-property", 2, 0, 0, (Scheme_function_unknown)ly_get_mus_property);
  scm_make_gsubr ("ly-set-mus-property", 3, 0, 0, (Scheme_function_unknown)ly_set_mus_property);
}
ADD_SCM_INIT_FUNC(musicscm,init_functions);
