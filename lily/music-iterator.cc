/*
  music-iterator.cc -- implement Music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

/*
  UGH. too many includes.
 */
#include "warn.hh"
#include "music-iterator.hh"
#include "translator-group.hh"
#include "music-wrapper.hh"
#include "music-wrapper-iterator.hh"
#include "simple-music-iterator.hh"
#include "context-specced-music.hh"

#include "ly-smobs.icc"

Music_iterator::Music_iterator ()
{
  smobify_self ();
}

Music_iterator::Music_iterator (Music_iterator const& src)
{
  handle_ = *src.handle_.clone ();
  music_ = src.music_;
  music_length_ = src.music_length_;
  start_mom_ = src.start_mom_;
  smobify_self ();
}

Music_iterator::~Music_iterator ()
{
}

Translator_group* 
Music_iterator::report_to () const
{
  return handle_.report_to ();
}

void
Music_iterator::set_translator (Translator_group *trans)
{
  handle_.set_translator (trans);
}

void
Music_iterator::construct_children ()
{
}

Moment
Music_iterator::pending_moment () const
{
  return 0;
}

void
Music_iterator::process (Moment)
{
}

bool
Music_iterator::ok () const
{
  return false;
}

void
Music_iterator::skip (Moment)
{
}

SCM
Music_iterator::get_pending_events (Moment)const
{
  return SCM_EOL;
}

SCM
Music_iterator::get_static_get_iterator (Music *m)
{
  Music_iterator * p =0;

  SCM ctor = m->get_mus_property ("iterator-ctor") ;
  SCM iter = SCM_EOL;
  if (gh_procedure_p (ctor))
    {
      iter = gh_call0 (ctor);
      p = unsmob_iterator (iter);
    }
  else
    {
      if (dynamic_cast<Music_wrapper *> (m))
	p = new Music_wrapper_iterator;
      else
	p = new Simple_music_iterator;

      iter = p->self_scm();
      scm_gc_unprotect_object (iter);
    }

  p->music_ = m;
  assert (m);
  p->music_length_ = m->length_mom ();
  p->start_mom_ = m->start_mom ();
  return iter;
}


Moment
Music_iterator::music_length_mom () const
{
  return music_length_;
}

Moment
Music_iterator::music_start_mom ()const
{
  return start_mom_;
}

void
Music_iterator::init_translator (Music *m, Translator_group *report)
{
  music_ = m;
  assert (m);
  if (Context_specced_music * csm =dynamic_cast<Context_specced_music *> (m))
    {
      SCM ct = csm->get_mus_property ("context-type");
      String c_type;
      if (gh_string_p (ct))
	  c_type =  ly_scm2string (ct);
      
      String c_id;
      SCM ci = csm->get_mus_property ("context-id");
      if (gh_string_p (ci))
	c_id = ly_scm2string (ci);
      
      Translator_group* a
	=report->find_create_translator (c_type, c_id);

      set_translator (a);
      
    }

  if (! report_to ())
    set_translator (report);
}


SCM
Music_iterator::get_iterator (Music *m) const
{
  SCM ip = get_static_get_iterator (m);
  Music_iterator*p = unsmob_iterator (ip);
  
  p->init_translator (m, report_to ());
  
  p->construct_children ();
  return ip;
}

/*
  TODO: rename to prevent confusion between Translator::try_music and
  Iterator::try_music
  
 */
Music_iterator*
Music_iterator::try_music (Music *m) const
{
  bool b = report_to ()->try_music ((Music*)m); // ugh
  Music_iterator * it = b ? (Music_iterator*) this : 0;	// ugh
  if (!it)
    it = try_music_in_children (m);
  return it;
}

Music_iterator*
Music_iterator::try_music_in_children (Music *) const
{
  return 0;
}

IMPLEMENT_CTOR_CALLBACK (Music_iterator);

Music *
Music_iterator::get_music () const
{
  return music_;
}

/****************************************************************/

IMPLEMENT_TYPE_P (Music_iterator, "ly-iterator?");
IMPLEMENT_SMOBS(Music_iterator);
IMPLEMENT_DEFAULT_EQUAL_P(Music_iterator);

SCM
Music_iterator::mark_smob (SCM smob)
{
  Music_iterator * mus = (Music_iterator *)SCM_CELL_WORD_1 (smob);

  mus->derived_mark ();
  /*
    Careful with GC, although we intend the following as pointers
    only, we _must_ mark them.
   */
  if (mus->report_to())
    scm_gc_mark (mus->report_to()->self_scm());
  if (mus->music_)
    scm_gc_mark (mus->music_->self_scm());
  

  return SCM_EOL;
}

int
Music_iterator::print_smob (SCM , SCM port, scm_print_state*)
{
  scm_puts ("#<Music iterator>", port);
  return 1;
}

void
Music_iterator::derived_mark()const
{
}

void
Music_iterator::quit ()
{
  handle_.quit ();
  do_quit ();
}

void
Music_iterator::do_quit()
{
}
