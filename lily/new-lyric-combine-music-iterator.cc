/*   
new-lyric-combine-iterator.cc --  implement New_lyric_combine_music_iterator

source file of the GNU LilyPond music typesetter

(c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

 */

#include "translator-group.hh"
#include "lyric-combine-music.hh"
#include "event.hh"
#include "grob.hh"
#include "music-iterator.hh"


class New_lyric_combine_music_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  New_lyric_combine_music_iterator ();
  New_lyric_combine_music_iterator (New_lyric_combine_music_iterator const&src);
  DECLARE_SCHEME_CALLBACK(constructor, ());
protected:
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit(); 
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;
  virtual bool run_always ()const;
  virtual bool ok () const;
  virtual void derived_mark () const;
  virtual void derived_substitute (Translator_group*,Translator_group*);
private:
  bool start_new_syllable () ;
  void find_thread ();
  
  Translator_group * lyrics_context_;
  Translator_group* music_context_;
  Music_iterator * lyric_iter_;
};

/*
  Ugh, why static?
 */
static Music *busy_ev;
static Music *start_ev;
static Music *melisma_playing_ev;

New_lyric_combine_music_iterator::New_lyric_combine_music_iterator ()
{
  lyric_iter_ =0;
  music_context_ =0;
  lyrics_context_ = 0;
  if (!busy_ev)
    {
      busy_ev
	= make_music_by_name (ly_symbol2scm ("BusyPlayingEvent"));
      start_ev
	= make_music_by_name (ly_symbol2scm ("StartPlayingEvent"));
      melisma_playing_ev
	= make_music_by_name (ly_symbol2scm ("MelismaPlayingEvent"));
    }
}

bool
New_lyric_combine_music_iterator::start_new_syllable ()
{
  bool b = music_context_->try_music (busy_ev);
  
  if (!b)
    return false;

  if (!to_boolean (lyrics_context_->get_property ("ignoreMelismata")))
    {
      bool m = music_context_->try_music (melisma_playing_ev);
      if (m)
	return false;
    }
  
  return true;
}

Moment
New_lyric_combine_music_iterator::pending_moment () const
{
  Moment m;

  m.set_infinite (1);
    
  return m;
}

bool
New_lyric_combine_music_iterator::run_always () const
{
  return true;
}

bool
New_lyric_combine_music_iterator::ok () const
{
  return lyric_iter_ && lyric_iter_->ok ();
}

void
New_lyric_combine_music_iterator::derived_mark()const
{
  if (lyric_iter_)
    scm_gc_mark (lyric_iter_->self_scm ());
  if (lyrics_context_)
    scm_gc_mark (lyrics_context_->self_scm ());
  if (music_context_)
    scm_gc_mark (music_context_->self_scm ());
}

void
New_lyric_combine_music_iterator::derived_substitute (Translator_group*f, Translator_group*t)
{
  if (lyric_iter_)
    lyric_iter_->substitute_outlet (f,t);
  if (lyrics_context_ && lyrics_context_==f)
    lyrics_context_ = t;
  if (music_context_ && music_context_ == f)
    music_context_ = t; 
}

/*
  ID == "" means accept any ID.
 */
Translator_group *
find_context_below (Translator_group * where,
		    String type, String id)
{
  if (where->context_name () == type)
    {
      if (id == "" || where->id_string_ == id)
	return where;
    }
  
  Translator_group * found = 0;
  for (SCM s = where->trans_group_list_;
       !found && gh_pair_p (s); s = gh_cdr (s))
    {
      Translator_group * tr = dynamic_cast<Translator_group*> (unsmob_translator (gh_car (s)));

      
      found = find_context_below (tr, type, id);
    }

  return found; 
}



void
New_lyric_combine_music_iterator::construct_children ()
{
  Music *m = unsmob_music (get_music ()->get_mus_property ("element"));
  lyric_iter_ = unsmob_iterator (get_iterator (m));

  if (lyric_iter_)
    {
      lyrics_context_ = find_context_below (lyric_iter_->report_to (), "LyricsVoice", "");
    }

  find_thread ();
}

void
New_lyric_combine_music_iterator::find_thread ()
{
  if (!music_context_)
    {
      SCM voice_name = get_music ()->get_mus_property ("associated-context");
  
      if (gh_string_p (voice_name))
	{
	  Translator_group *t = report_to ();
	  while (t && t->daddy_trans_)
	    t = t->daddy_trans_;

	  String name =  ly_scm2string (voice_name);
	  Translator_group* voice = find_context_below (t, "Voice", name);
	  Translator_group *thread = 0;
	  if (voice)
	    thread = find_context_below (voice, "Thread", "");
	  else
	    get_music ()->origin ()->warning (_f("Cannot find Voice: %s\n", name.to_str0())); 

	  if (thread)
	    music_context_ = thread;
	    
	  if (lyrics_context_ && voice)
	    lyrics_context_->set_property ("associatedVoiceContext",  voice->self_scm ());
	}
    }
}

void
New_lyric_combine_music_iterator::process (Moment )
{
  find_thread ();
  if (!music_context_)
    return ;
  
  if (!music_context_->daddy_trans_)
    {
      music_context_ = 0;
      if (lyrics_context_)
	lyrics_context_->unset_property (ly_symbol2scm ("associatedVoiceContext"));
    }
  
  if (music_context_
      && start_new_syllable () && lyric_iter_->ok ())
    {
      Moment m= lyric_iter_->pending_moment ();
      lyric_iter_->process (m);
    }
}

void
New_lyric_combine_music_iterator::do_quit ()
{
  if (lyric_iter_)
    lyric_iter_->quit();
}

New_lyric_combine_music_iterator::New_lyric_combine_music_iterator (New_lyric_combine_music_iterator const & src)
    : Music_iterator (src)
{
  lyric_iter_ = 0;

  if (src.lyric_iter_)
    lyric_iter_ =  src.lyric_iter_->clone ();

  if (lyric_iter_)
    scm_gc_unprotect_object (lyric_iter_->self_scm());

  music_context_ = src.music_context_;
  lyric_iter_ = src.lyric_iter_;
    
  
  assert (false);		// shouldn't copy, really.
}


Music_iterator*
New_lyric_combine_music_iterator::try_music_in_children (Music *m) const
{
  return lyric_iter_->try_music (m);
}


IMPLEMENT_CTOR_CALLBACK (New_lyric_combine_music_iterator);
