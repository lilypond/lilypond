/*
  music.cc -- implement Music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "input-smob.hh"
#include "music.hh"
#include "music-list.hh"
#include "debug.hh"
#include "musical-pitch.hh"
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


Music::Music()
{
  immutable_property_alist_ = SCM_EOL;
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

void
Music::do_print() const
{
}

Moment
Music::length_mom () const
{
  return 0;
}

int
Music::print_smob(SCM s, SCM p, scm_print_state*)
{
  scm_puts ("#<Music ", p);
  scm_puts (classname(unsmob_music (s)),p);
  scm_puts (">",p);
  return 1;
}

Musical_pitch
Music::to_relative_octave (Musical_pitch m)
{
  return m;
}


void
Music::transpose (Musical_pitch )
{
}


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


void
Music::print ()const
{
}



Music::~Music ()
{
  
}
