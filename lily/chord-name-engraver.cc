/*
  chord-name-engraver.cc -- implement Chord_name_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "chord-name.hh"
#include "chord.hh"
#include "musical-request.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "paper-def.hh"
#include "main.hh"
#include "dimensions.hh"
#include "item.hh"
#include "pitch.hh"
#include "protected-scm.hh"

class Chord_name_engraver : public Engraver 
{
public:
  Chord_name_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info i);
  virtual void do_process_music ();
  virtual bool do_try_music (Music* m);

private:
  void create_chord_name ();
  
  Item* chord_name_p_;
  Protected_scm pitches_;

  Protected_scm chord_;
  Protected_scm last_chord_;

  Protected_scm tonic_req_;
  Protected_scm inversion_req_;
  Protected_scm bass_req_;
};

ADD_THIS_TRANSLATOR (Chord_name_engraver);

Chord_name_engraver::Chord_name_engraver ()
{
  chord_name_p_ = 0;
  pitches_ = SCM_EOL;
  tonic_req_ = SCM_EOL;
  inversion_req_ = SCM_EOL;
  bass_req_ = SCM_EOL;
  chord_ = SCM_EOL;
  last_chord_ = SCM_EOL;
}

void
Chord_name_engraver::acknowledge_element (Score_element_info i)
{
  if (Note_req* n = dynamic_cast<Note_req*> (i.req_l_))
    pitches_ = gh_cons (n->get_mus_property ("pitch"), pitches_);
}

bool
Chord_name_engraver::do_try_music (Music* m)
{
  if (Note_req* n = dynamic_cast<Note_req*> (m))
    {
      pitches_ = gh_cons (n->get_mus_property ("pitch"), pitches_);
      return true;
    }
  if (Tonic_req* t = dynamic_cast<Tonic_req*> (m))
    {
      tonic_req_ = t->get_mus_property ("pitch");
      return true;
    }
  if (Inversion_req* i = dynamic_cast<Inversion_req*> (m))
    {
      inversion_req_ = i->get_mus_property ("pitch");
      return true;
    }
  if (Bass_req* b = dynamic_cast<Bass_req*> (m))
    {
      bass_req_ = b->get_mus_property ("pitch");
      return true;
    }
  return false;
}

void
Chord_name_engraver::do_process_music ()
{
  if (!chord_name_p_ && pitches_ != SCM_EOL)
    {
      bool find_inversion_b = false;
      SCM chord_inversion = get_property ("chordInversion");
      if (gh_boolean_p (chord_inversion))
	find_inversion_b = gh_scm2bool (chord_inversion);

      chord_ = Chord::pitches_and_requests_to_chord (pitches_,
						     tonic_req_,
						     inversion_req_,
						     bass_req_,
						     find_inversion_b);
      
      create_chord_name ();
      announce_element (chord_name_p_, 0);
      SCM s = get_property ("drarnChords"); //FIXME!
      if (to_boolean (s) && last_chord_ != SCM_EOL &&
	  gh_equal_p (chord_, last_chord_))
	chord_name_p_->set_elt_property ("begin-of-line-visible", SCM_BOOL_T);
    }
}

void
Chord_name_engraver::create_chord_name ()
{
  chord_name_p_ = new Item (get_property ("ChordName"));

  SCM pitches = gh_car (chord_);
  SCM modifiers = gh_cdr (chord_);
  SCM inversion = gh_car (modifiers);
  SCM bass = gh_cdr (modifiers);
  /* Hmm, maybe chord-name should use (pitches (inversion . base)) too? */
  chord_name_p_->set_elt_property ("pitches", pitches);
  chord_name_p_->set_elt_property ("inversion", inversion);
  chord_name_p_->set_elt_property ("inversion", bass);
}

void
Chord_name_engraver::do_pre_move_processing ()
{
  if (chord_name_p_)
    {
      typeset_element (chord_name_p_);
    }
  chord_name_p_ = 0;

  pitches_ = SCM_EOL;
  tonic_req_ = SCM_EOL;
  inversion_req_ = SCM_EOL;
  bass_req_ = SCM_EOL;
  last_chord_ = chord_;
  chord_ = SCM_EOL;
}

