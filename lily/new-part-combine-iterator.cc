/*   
     new-part-combine-music-iterator.cc -- implement New_pc_iterator

     source file of the GNU LilyPond music typesetter
  
     (c) 2004 Han-Wen Nienhuys
*/

#include "translator-group.hh"
#include "event.hh"
#include "music-sequence.hh"
#include "lily-guile.hh"
#include "warn.hh"
#include "music-iterator.hh"
#include "interpretation-context-handle.hh"

class New_pc_iterator : public Music_iterator
{
public:
  New_pc_iterator ();

  DECLARE_SCHEME_CALLBACK(constructor, ()); 
protected:
  virtual void derived_substitute (Translator_group*f, Translator_group*t) ;
  virtual void derived_mark () const;
  New_pc_iterator (New_pc_iterator const &);

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit(); 
  virtual void process (Moment);

  virtual Music_iterator *try_music_in_children (Music *) const;

  virtual bool ok () const;

private:
  Music_iterator * first_iter_;
  Music_iterator * second_iter_;
  
  SCM split_list_;

  enum Status  {
    APART, TOGETHER,
    SOLO1, SOLO2,
    UNISONO, UNISILENCE,
  };
  Status state_;
  Status playing_state_;

  Interpretation_context_handle one_;
  Interpretation_context_handle two_;
  Interpretation_context_handle null_;
  Interpretation_context_handle shared_;

  void kill_mmrest (Translator_group*);
  void chords_together ();
  void solo1 ();
  void solo2 ();
  void apart (bool silent);
  void unisono (bool silent);
};


New_pc_iterator::New_pc_iterator ()
{
  first_iter_ = 0;
  second_iter_ = 0;
  split_list_ = SCM_EOL;
  state_ = APART;
  playing_state_ = APART;
}

void
New_pc_iterator::derived_mark () const
{
  if (first_iter_)
    scm_gc_mark (first_iter_->self_scm());
  if (second_iter_)
    scm_gc_mark(second_iter_->self_scm());
}

void
New_pc_iterator::derived_substitute (Translator_group*f,
				     Translator_group*t)
{
  if (first_iter_)
    first_iter_->substitute_outlet (f,t);
  if (second_iter_)
    second_iter_->substitute_outlet (f,t);
}

void
New_pc_iterator::do_quit ()
{
  if (first_iter_)
    first_iter_->quit();
  if (second_iter_)
    second_iter_->quit();

  one_ .set_translator (0);
  two_.set_translator (0);
  shared_.set_translator (0);
}



Moment
New_pc_iterator::pending_moment () const
{
  Moment p;
  p.set_infinite (1);
  if (first_iter_->ok ())
    p = p <? first_iter_->pending_moment ();

  if (second_iter_->ok ())
    p = p <? second_iter_->pending_moment ();
  return p;
}

bool
New_pc_iterator::ok () const
{
  return first_iter_->ok () || second_iter_->ok ();
}

void
New_pc_iterator::chords_together ()
{
  if (state_ == TOGETHER)
    return;
  else
    {
      playing_state_ = TOGETHER;
      state_ = TOGETHER;
      first_iter_->substitute_outlet (one_.get_outlet (), shared_.get_outlet ());
      first_iter_->substitute_outlet (null_.get_outlet (), shared_.get_outlet ());
      second_iter_->substitute_outlet (two_.get_outlet (), shared_.get_outlet ());
      second_iter_->substitute_outlet (null_.get_outlet (), shared_.get_outlet ());
    }
}


void
New_pc_iterator::kill_mmrest (Translator_group * tg)
{
  static Music * mmrest;
  if (!mmrest)
    {
      mmrest = make_music_by_name (ly_symbol2scm ("MultiMeasureRestEvent"));
      mmrest->set_mus_property ("duration", SCM_EOL);
    }

  tg->try_music (mmrest);
}

void
New_pc_iterator::solo1 ()
{
  if (state_ == SOLO1)
    return;
  else
    {
      state_ = SOLO1;
      first_iter_->substitute_outlet (null_.get_outlet (), shared_.get_outlet ());
      first_iter_->substitute_outlet (one_.get_outlet (), shared_.get_outlet ());

      second_iter_->substitute_outlet (two_.get_outlet (), null_.get_outlet ());
      second_iter_->substitute_outlet (shared_.get_outlet (), null_.get_outlet ());
      kill_mmrest (two_.get_outlet ());
      kill_mmrest (shared_.get_outlet ());

      if (playing_state_ != SOLO1)
	{
	  static Music* event;
	  if (!event)
	    event = make_music_by_name (ly_symbol2scm ("SoloOneEvent"));

	  first_iter_-> try_music_in_children (event);
	}
      playing_state_ = SOLO1;
    }
}
void
New_pc_iterator::unisono (bool silent)
{
  Status newstate = (silent) ? UNISILENCE : UNISONO;
  
  if (newstate == state_)
    return; 
  else
    {

      first_iter_->substitute_outlet (null_.get_outlet (), shared_.get_outlet ());
      first_iter_->substitute_outlet (one_.get_outlet (), shared_.get_outlet ());

      second_iter_->substitute_outlet (two_.get_outlet (), null_.get_outlet ());
      second_iter_->substitute_outlet (shared_.get_outlet (), null_.get_outlet ());
      kill_mmrest (two_.get_outlet ());
      kill_mmrest (shared_.get_outlet ());

      if (playing_state_ != UNISONO
	  && newstate == UNISONO)
	{
	  static Music* event;
	  if (!event)
	    event = make_music_by_name (ly_symbol2scm ("UnisonoEvent"));

	  first_iter_-> try_music_in_children (event);      
	  playing_state_ = UNISONO;
	}
      state_ = newstate;
    }
}

void
New_pc_iterator::solo2 ()
{
  if (state_ == SOLO2)
    return;
  else
    {
      state_ = SOLO2;
      
      second_iter_->substitute_outlet (null_.get_outlet (), shared_.get_outlet ());
      second_iter_->substitute_outlet (two_.get_outlet (), shared_.get_outlet ());

      first_iter_->substitute_outlet (one_.get_outlet (), null_.get_outlet ());
      first_iter_->substitute_outlet (shared_.get_outlet (), null_.get_outlet ());
      kill_mmrest (one_.get_outlet ());
      kill_mmrest (shared_.get_outlet ());
      
      if (playing_state_ != SOLO2)
	{
	  static Music* event;
	  if (!event)
	    event = make_music_by_name (ly_symbol2scm ("SoloTwoEvent"));

	  second_iter_-> try_music_in_children (event);
	  playing_state_ = SOLO2;
	}
    }
}

void
New_pc_iterator::apart (bool silent)
{
  if (!silent)
    playing_state_ = APART;

  if (state_ == APART)
    return;
  else
    {
      state_ = APART;
  
      first_iter_->substitute_outlet (null_.get_outlet (), one_.get_outlet ());
      first_iter_->substitute_outlet (shared_.get_outlet (), one_.get_outlet ());
  
      second_iter_->substitute_outlet (null_.get_outlet (), two_.get_outlet ());
      second_iter_->substitute_outlet (shared_.get_outlet (), two_.get_outlet ());    }
}

void
New_pc_iterator::construct_children ()
{
  split_list_ =  get_music ()->get_mus_property ("split-list");
  SCM lst =  get_music ()->get_mus_property ("elements");

  SCM props = scm_list_n (scm_list_n (ly_symbol2scm ("denies"), ly_symbol2scm ("Thread"), SCM_UNDEFINED),
			  scm_list_n (ly_symbol2scm ("consists"), ly_symbol2scm ("Rest_engraver"), SCM_UNDEFINED),
			  scm_list_n (ly_symbol2scm ("consists"), ly_symbol2scm ("Note_heads_engraver"), SCM_UNDEFINED),
			  SCM_UNDEFINED);

  Translator_group *tr
    =  get_outlet ()->find_create_translator (ly_symbol2scm ("Voice"),
					     "shared",props);

  shared_ .set_translator (tr); 
  set_translator (tr);
  Translator_group *null
    =  get_outlet ()->find_create_translator (ly_symbol2scm ("Devnull"),
					     "", SCM_EOL);

  if (!null)
    programming_error ("No Devnull found?");
  
  null_.set_translator (null);

  Translator_group *one = tr->find_create_translator (ly_symbol2scm ("Voice"),
						      "one", props);

  one_.set_translator (one);

  set_translator (one);
  first_iter_ = unsmob_iterator (get_iterator (unsmob_music (gh_car (lst))));


  Translator_group *two = tr->find_create_translator (ly_symbol2scm ("Voice"),
						      "two", props);
  two_.set_translator (two);
  set_translator (two);
  second_iter_ = unsmob_iterator (get_iterator (unsmob_music (gh_cadr (lst))));


  set_translator (tr);


  char const * syms[] = {
    "Stem",
    "DynamicLineSpanner",
    "Tie",
    "Dots",
    "Slur",
    "TextScript",
    "Script",
    0
  };
  
  for (char const**p = syms; *p; p++)
    {
      SCM sym = ly_symbol2scm (*p);
      one->execute_pushpop_property (sym,
				     ly_symbol2scm ("direction"), gh_int2scm (1));

      two->execute_pushpop_property (sym,
				     ly_symbol2scm ("direction"), gh_int2scm (-1));
    }

}

void
New_pc_iterator::process (Moment m)
{
  Moment now = get_outlet ()->now_mom ();
  Moment *splitm = 0;
  
  for (; gh_pair_p (split_list_); split_list_ = gh_cdr (split_list_))
    {
      splitm = unsmob_moment (gh_caar (split_list_));
      if (*splitm > now)
	break ;

      SCM tag = gh_cdar (split_list_);
      
      if (tag == ly_symbol2scm ("chords"))
	chords_together ();
      else if (tag == ly_symbol2scm ("apart")
	       || tag == ly_symbol2scm ("apart-silence")
	       || tag == ly_symbol2scm ("apart-spanner"))
	apart (tag == ly_symbol2scm ("apart-silence"));
      else if (tag == ly_symbol2scm ("unisono"))
	unisono (false);
      else if (tag == ly_symbol2scm ("unisilence"))
	unisono (true);
      else if (tag == ly_symbol2scm ("solo1"))
	solo1 ();
      else if (tag == ly_symbol2scm ("solo2"))
	solo2 ();
      else
	{
	  String s =  "Unknown split directive: "
	    + (gh_symbol_p (tag) ? ly_symbol2string (tag) : String ("not a symbol")); 
	  programming_error (s);
	}
    }
  
  if (first_iter_->ok ())
    first_iter_->process (m);
  
  if (second_iter_->ok ())
    second_iter_->process (m);
}

Music_iterator*
New_pc_iterator::try_music_in_children (Music *m) const
{
  Music_iterator * i =  first_iter_->try_music (m);
  if (i)
    return i;
  else
    return second_iter_->try_music (m);
}

IMPLEMENT_CTOR_CALLBACK (New_pc_iterator);
