/*
  Simultaneous_music-iterator.cc -- implement Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "warn.hh"
#include "simultaneous-music-iterator.hh"
#include "music-list.hh"
#include "killing-cons.tcc"


Simultaneous_music_iterator::Simultaneous_music_iterator ()
{
  separate_contexts_b_ = false;
  children_list_ = SCM_EOL;
}

Simultaneous_music_iterator::Simultaneous_music_iterator (Simultaneous_music_iterator const& src)
  : Music_iterator (src)
{
  separate_contexts_b_
    = src.separate_contexts_b_;
  children_list_ = SCM_EOL;
  
  SCM children_list = SCM_EOL;
  SCM *tail  = &children_list; 
  for (SCM s = src.children_list_; gh_pair_p (s); s = gh_cdr(s))
    {
      Music_iterator *i = unsmob_iterator (gh_car (s));
      SCM cl = i->clone ()->self_scm();
      *tail = scm_cons (cl, *tail);
      tail = SCM_CDRLOC (*tail);
      scm_gc_unprotect_object (cl);
    }

  children_list_ = children_list;
  scm_remember_upto_here_1 (children_list);
}

void
Simultaneous_music_iterator::derived_mark()const
{
  scm_gc_mark (children_list_);
}

SCM
Simultaneous_music_iterator::get_pending_events (Moment m)const
{
  SCM l = SCM_EOL;
  for (SCM s = children_list_; gh_pair_p (s); s = gh_cdr(s))
    {
      l = gh_append2 (unsmob_iterator (gh_car (s))->get_pending_events (m), l);
    }
  return l;
}

void
Simultaneous_music_iterator::construct_children ()
{
  int j = 0;

  SCM i = get_music ()->get_mus_property ("elements");

  children_list_ = SCM_EOL;
  SCM * tail = &children_list_;
  for (; gh_pair_p (i); i = ly_cdr (i), j++)
    {
      Music *mus = unsmob_music (ly_car (i));

      SCM scm_iter = get_static_get_iterator (mus);
      Music_iterator * mi = unsmob_iterator (scm_iter);

      /* if separate_contexts_b_ is set, create a new context with the
	 number number as name */
      
      Translator_group * t = (j && separate_contexts_b_)
	? report_to ()->find_create_translator (report_to ()->type_string_,
						    to_string (j))
	: report_to ();

      if (!t)
	t = report_to ();

      mi->init_translator (mus, t);
      mi->construct_children ();

      if (mi->ok ()) 
	{
	  *tail = scm_cons (scm_iter, *tail);
	  tail = SCM_CDRLOC (*tail);
	}
      else
	mi->set_translator (0);
    }
}

void
Simultaneous_music_iterator::process (Moment until)
{
  SCM *proc = &children_list_; 
  while(gh_pair_p (*proc))
    {
      Music_iterator * i = unsmob_iterator (gh_car (*proc));
      if (i->pending_moment () == until) 
	{
	  i->process (until);
	}
      if (!i->ok ())
	{
	  i->quit ();
	  *proc = gh_cdr (*proc);
	}
      else
	{
	  proc = SCM_CDRLOC(*proc);
	}
    }
}

void
Simultaneous_music_iterator::skip (Moment until)
{
  SCM *proc = &children_list_; 
  while(gh_pair_p (*proc))
    {
      Music_iterator * i = unsmob_iterator (gh_car (*proc));
      if (i->pending_moment () <= until) 
	{
	  i->skip (until);
	}
      if (!i->ok ())
	{
	  i->quit ();
	  *proc = gh_cdr (*proc);
	}
      else
	{
	  proc = SCM_CDRLOC(*proc);
	}
    }
}

Moment
Simultaneous_music_iterator::pending_moment () const
{
  Moment next;
  next.set_infinite (1);
  
  for (SCM s = children_list_; gh_pair_p (s); s = gh_cdr(s))
    next = next <? unsmob_iterator (gh_car (s))->pending_moment () ;
  return next;
}



bool
Simultaneous_music_iterator::ok () const
{
  return gh_pair_p (children_list_);
}

Music_iterator*
Simultaneous_music_iterator::try_music_in_children (Music *m) const
{
  Music_iterator * b=0;
  for (SCM s = children_list_; !b && gh_pair_p (s); s = gh_cdr(s))
    b =unsmob_iterator (gh_car (s))->try_music (m);
  return b;
}

void
Simultaneous_music_iterator::do_quit ()
{
  for (SCM s = children_list_; gh_pair_p (s); s = gh_cdr(s))
    unsmob_iterator (gh_car (s))->quit();
}


IMPLEMENT_CTOR_CALLBACK (Simultaneous_music_iterator);
