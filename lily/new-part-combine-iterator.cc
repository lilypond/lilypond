/*   
  part-combine-music-iterator.cc -- implement  New_pc_iterator

  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2003 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "part-combine-music-iterator.hh"
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
  VIRTUAL_COPY_CONS (Music_iterator);
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

  virtual SCM get_pending_events (Moment)const;
  virtual Music_iterator *try_music_in_children (Music *) const;

  virtual bool ok () const;

private:
  Music_iterator * first_iter_;
  Music_iterator * second_iter_;
  bool is_shared_ ;
  SCM split_list_;

  Interpretation_context_handle one_;
  Interpretation_context_handle two_;
  Interpretation_context_handle shared_;

  void together ();
  void apart ();
};


New_pc_iterator::New_pc_iterator ()
{
  is_shared_  =false;
  first_iter_ = 0;
  second_iter_ = 0;
  split_list_ = SCM_EOL;
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

New_pc_iterator::New_pc_iterator (New_pc_iterator const &src)
  : Music_iterator (src)
{
  first_iter_ = 0;
  second_iter_ = 0;

  if(src.first_iter_)
    first_iter_ = src.first_iter_->clone ();
  if (src.second_iter_)
    second_iter_ = src.second_iter_->clone ();

  split_list_ = src.split_list_;
  
  if (first_iter_)
    scm_gc_unprotect_object (first_iter_->self_scm());
  if (second_iter_)
    scm_gc_unprotect_object (second_iter_->self_scm());
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
New_pc_iterator::together ()
{
  first_iter_->substitute_outlet (one_.report_to (), shared_.report_to ());
  second_iter_->substitute_outlet (two_.report_to (), shared_.report_to ());
}

void
New_pc_iterator::apart ()
{
  first_iter_->substitute_outlet (shared_.report_to (),one_.report_to ());
  second_iter_->substitute_outlet (shared_.report_to (), two_.report_to ());
}


void
New_pc_iterator::construct_children ()
{
  split_list_ =  get_music ()->get_mus_property ("split-list");
  SCM lst =  get_music ()->get_mus_property ("elements");

  Translator_group *tr
    =  report_to ()->find_create_translator (ly_symbol2scm ("Voice"),
					     "shared", SCM_EOL);

  tr->execute_pushpop_property (ly_symbol2scm ("NoteHead"),
				ly_symbol2scm ("font-size"), gh_int2scm (3));

  
  shared_ .set_translator (tr); 
  set_translator (tr);

  Translator_group *one = tr->find_create_translator (ly_symbol2scm ("Voice"),
						      "one", SCM_EOL);

  one_.set_translator (one);
  one->execute_pushpop_property (ly_symbol2scm ("Stem"),
				 ly_symbol2scm ("direction"), gh_int2scm (1));

  set_translator (one);
  first_iter_ = unsmob_iterator (get_iterator (unsmob_music (gh_car (lst))));


  Translator_group *two = tr->find_create_translator (ly_symbol2scm ("Voice"),
						      "two", SCM_EOL);
  two_.set_translator (two);
  two_.report_to ()->execute_pushpop_property (ly_symbol2scm ("Stem"),
				  ly_symbol2scm ("direction"), gh_int2scm (-1));
  set_translator (two);
  second_iter_ = unsmob_iterator (get_iterator (unsmob_music (gh_cadr (lst))));


  set_translator (tr);
}

void
New_pc_iterator::process (Moment m)
{
  Moment now = report_to ()->now_mom ();
  Moment *splitm = 0;
  
  for (; gh_pair_p (split_list_); split_list_ = gh_cdr (split_list_))
    {
      splitm = unsmob_moment (gh_caar (split_list_));
      if (*splitm > now)
	break ;

      if (gh_cdar (split_list_) == ly_symbol2scm ("together"))
	together ();
      else if (gh_cdar (split_list_) == ly_symbol2scm ("apart"))
	apart ();
      else
	programming_error ("Unknown split directive.");
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


SCM
New_pc_iterator::get_pending_events (Moment m)const
{
  SCM s = SCM_EOL;
  if (first_iter_)
    s = gh_append2 (s,first_iter_->get_pending_events (m));
  if (second_iter_)
    s = gh_append2 (second_iter_->get_pending_events (m),s);
  return s;
}

IMPLEMENT_CTOR_CALLBACK (New_pc_iterator);
