/*
  music.cc -- implement Music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "main.hh"
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
      scm_gc_unprotect_object (ss);
      return ss;
    }
  else if (gh_pair_p (m))
    {
      return gh_cons (ly_deep_mus_copy (ly_car (m)), ly_deep_mus_copy (ly_cdr (m)));
    }
  else
    return m;
}


Music::Music ()
{
  immutable_property_alist_ = SCM_EOL;
  mutable_property_alist_ = SCM_EOL;
  smobify_self ();
}

Music::Music (Music const &m)
{
  immutable_property_alist_ = m.immutable_property_alist_;
  SCM c =ly_deep_mus_copy (m.mutable_property_alist_);
  mutable_property_alist_ = c;

  smobify_self ();

  set_spot (*m.origin ());
}


Music::Music (SCM l)
{
  immutable_property_alist_ = l;
  mutable_property_alist_ = SCM_EOL;
  smobify_self ();
}


SCM
Music::mark_smob (SCM m)
{
  Music * mus = (Music *)SCM_CELL_WORD_1 (m);
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
    return *unsmob_moment (l);
  else if (gh_procedure_p (l))
    {
      SCM res = gh_call1 (l, self_scm ());
      return *unsmob_moment (res);
    }
    
  return 0;
}

Moment
Music::start_mom () const
{
  Moment m ;
  return m;
}

void
print_alist (SCM a, SCM port)
{
  for (SCM s = a; gh_pair_p (s); s = ly_cdr (s))
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
  scm_puts (classname (m),p);

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
Music::transpose (Pitch delta)
{
  Pitch *p = unsmob_pitch (get_mus_property ("pitch"));
  if (!p)
    return ;

  Pitch np = *p;
  np.transpose (delta);
  
  if (abs (np.alteration_i_) > 2)
    {
	warning (_f ("Transposition by %s makes accidental larger than two",
	  delta.str ()));
    }

  set_mus_property ("pitch", np.smobbed_copy ());
}

IMPLEMENT_TYPE_P (Music, "music?");

IMPLEMENT_SMOBS (Music);
IMPLEMENT_DEFAULT_EQUAL_P (Music);

/****************************/

SCM
Music::internal_get_mus_property (SCM sym) const
{
  SCM s = scm_sloppy_assq (sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return ly_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  return (s == SCM_BOOL_F) ? SCM_EOL : ly_cdr (s); 
}



void
Music::internal_set_mus_property (SCM s, SCM v)
{
#ifndef NDEBUG
  if (internal_type_checking_global_b)
    assert (type_check_assignment (s, v, ly_symbol2scm ("music-type?")));
#endif
  

  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, s, v);
}

#include "main.hh"

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
      return sc->internal_get_mus_property (sym);
    }
  else
    {
      warning (_ ("ly_get_mus_property (): Not a Music"));
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
      warning (_ ("ly_set_mus_property (): Not a symbol"));
      scm_write (mus, scm_current_error_port ());      

      return SCM_UNSPECIFIED;
    }

  if (!sc)
    {
      warning (_ ("ly_set_mus_property ():  not of type Music: "));
      scm_write (mus, scm_current_error_port ());
      return SCM_UNSPECIFIED;
    }


  bool ok = type_check_assignment (sym, val, ly_symbol2scm ("music-type?"));
  if (ok)
    {
      sc->internal_set_mus_property (sym, val);
    }
    
  return SCM_UNSPECIFIED;
}


// to do  property args 
SCM
ly_make_music (SCM type)
{
  if (!gh_string_p (type))
    {
      warning (_ ("ly_make_music (): Not a string"));
      scm_write (type, scm_current_error_port ());      

      return SCM_UNSPECIFIED;
    }
  else
    {
      SCM s = get_music (ly_scm2string (type))->self_scm ();
      scm_gc_unprotect_object (s);

      return s;
    }
}

SCM
ly_music_name (SCM mus)
{
  Music * m = unsmob_music (mus);
  const char *nm ="";
  if (!m)
    {
      warning (_ ("ly_music_name (): Not a music expression"));
      scm_write (mus, scm_current_error_port ());      
    }
   else
     nm = classname (m);
   return ly_str02scm (nm);
}

SCM
ly_music_list_p (SCM l)
{
  if (scm_list_p (l) != SCM_BOOL_T)
    return SCM_BOOL_F;

  while (gh_pair_p (l))
    {
      if (!unsmob_music (gh_car (l)))
	return SCM_BOOL_F;
      l =gh_cdr (l);
    }
  return SCM_BOOL_T;
}

static void
init_functions ()
{
  scm_c_define_gsubr ("music-list?", 1, 0, 0, (Scheme_function_unknown)ly_music_list_p);  
  scm_c_define_gsubr ("ly-get-mus-property", 2, 0, 0, (Scheme_function_unknown)ly_get_mus_property);
  scm_c_define_gsubr ("ly-set-mus-property", 3, 0, 0, (Scheme_function_unknown)ly_set_mus_property);
  scm_c_define_gsubr ("ly-make-music", 1, 0, 0, (Scheme_function_unknown)ly_make_music);
  scm_c_define_gsubr ("ly-music-name", 1, 0, 0, (Scheme_function_unknown)ly_music_name);    
}
ADD_SCM_INIT_FUNC (musicscm,init_functions);
ADD_MUSIC(Music);
