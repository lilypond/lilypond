/*
  head-grav.cc -- part of GNU LilyPond

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <ctype.h>

#include "rhythmic-head.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "dots.hh"
#include "dot-column.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "score-engraver.hh"
#include "warn.hh"

/*

  How does this work?

  When we catch the note, we predict the end of the note. We keep the
  requests living until we reach the predicted end-time.

  Every time process_music() is called and there are note requests, we
  figure out how long the note to typeset should be. It should be no
  longer than what's specified, than what is left to do and it should
  not cross barlines.
  
  We copy the reqs into scratch note reqs, to make sure that we get
  all durations exactly right.
 */

class Completion_heads_engraver : public Engraver
{
  Link_array<Item> note_p_arr_;
  
  Link_array<Item> dot_p_arr_;
  Link_array<Music> note_req_l_arr_;
  Link_array<Music> scratch_note_reqs_;

  Moment note_end_mom_;
  bool first_b_;
  Rational left_to_do_;

  Moment next_barline_moment ();
  Duration find_nearest_duration (Rational length);
  
public:
  TRANSLATOR_DECLARATIONS(Completion_heads_engraver);

protected:
  virtual void initialize ();
  virtual void start_translation_timestep ();
  virtual bool try_music (Music *req_l) ;
  virtual void process_music ();
  virtual void stop_translation_timestep ();
};

void
Completion_heads_engraver::initialize ()
{
  first_b_ = false;
}

bool
Completion_heads_engraver::try_music (Music *m) 
{
  if (Note_req * n =dynamic_cast <Note_req *> (m))
    {
      note_req_l_arr_.push (n);

      first_b_ = true;
      Moment musiclen = m->length_mom ();
      Moment now = now_mom();

      if (now_mom ().grace_part_)
	{
	  musiclen.grace_part_ = musiclen.main_part_ ;
	  musiclen.main_part_ = Rational (0,1);
	}
      note_end_mom_  = note_end_mom_ >? (now + musiclen);
      return true;
    }
  else if (dynamic_cast<Busy_playing_req*> (m))
    {
      return note_req_l_arr_.size ();
    }
  
  return false;
  
}

Moment
Completion_heads_engraver::next_barline_moment ( )
{
  Moment *e = unsmob_moment (get_property ("measurePosition"));
  Moment *l = unsmob_moment (get_property ("measureLength"));
  if (!e || !l)
    {
      programming_error ("No timing props set?");
      return Moment (1,1);
    }

  return (*l - *e);
}

Duration  
Completion_heads_engraver::find_nearest_duration (Rational length)
{
  int log_limit= 6;

  Duration d(0,0);

  /*
    this could surely be done more efficient. Left to the reader as an
    excercise.  */
  while (d.length_mom () > length && d.duration_log () < log_limit)
    {
      if (d.dot_count ())
	{
	  d = Duration (d.duration_log (), d.dot_count ()- 1);
	  continue;
	}
      else
	{
	  d = Duration (d.duration_log () + 1, 2);
	}
    }

  if (d.duration_log () >= log_limit)
    {
      // junk the dots.
      d = Duration (d.duration_log (), 0);

      // scale up.
      d = d.compressed (length / d.length_mom ());
    }
  
  return d;
}

void
Completion_heads_engraver::process_music ()
{
  if (!first_b_ && !left_to_do_)
    return ;

  first_b_ = false;
  
  Duration note_dur;
  Duration *orig = 0;
  if (left_to_do_)
    {
      note_dur = find_nearest_duration (left_to_do_);
    }
  else
    {
      orig = unsmob_duration (note_req_l_arr_[0]->get_mus_property ("duration"));
      note_dur = *orig;
    }

  Moment nb = next_barline_moment ();
  if (nb < note_dur.length_mom ())
    {
      note_dur = find_nearest_duration (nb.main_part_);

      Moment next = now_mom();
      next.main_part_ += note_dur.length_mom ();
      top_engraver ()->add_moment_to_process (next);
    }

  if (orig)
    {
      left_to_do_ = orig->length_mom ();
    }

  if (orig && note_dur.length_mom() != orig->length_mom())
    {
      if (!scratch_note_reqs_.size ())
	for (int i = 0; i < note_req_l_arr_.size (); i++)
	  {
	    Music * m = note_req_l_arr_[i]->clone ();
	    scratch_note_reqs_.push (m);
	  }

      for (int i =0; i < scratch_note_reqs_.size (); i++)
	scratch_note_reqs_[i]->set_mus_property ("duration", note_dur.smobbed_copy ());
    }

  
  for (int i = 0;
       left_to_do_ && i < note_req_l_arr_.size (); i++)
    {
      Item *note_p  = new Item (get_property ("NoteHead"));
      
      Music * req =  note_req_l_arr_[i];
      if (scratch_note_reqs_.size())
	{
	  req = scratch_note_reqs_[i];
	  SCM pits = note_req_l_arr_[i]->get_mus_property ("pitch");
	  req->set_mus_property ("pitch",pits);
	}

      note_p->set_grob_property ("duration-log",
				 gh_int2scm (note_dur.duration_log ()));
      
      int dots= note_dur.dot_count ();
      if (dots)
	{
	  Item * d = new Item (get_property ("Dots"));
	  Rhythmic_head::set_dots (note_p, d);

	  /*
	   measly attempt to save an eeny-weenie bit of memory.
	  */
	  if (dots != gh_scm2int (d->get_grob_property ("dot-count")))
	    d->set_grob_property ("dot-count", gh_int2scm (dots));

	  d->set_parent (note_p, Y_AXIS);
	  announce_grob (d, SCM_EOL);
	  dot_p_arr_.push (d);
	}

      Pitch *pit =unsmob_pitch (req->get_mus_property ("pitch"));

      int pos = pit->steps ();
      SCM c0 = get_property ("centralCPosition");
      if (gh_number_p (c0))
	pos += gh_scm2int (c0);

      note_p->set_grob_property ("staff-position",   gh_int2scm (pos));
      announce_grob (note_p,req->self_scm ());
      note_p_arr_.push (note_p);
    }

  left_to_do_ -= note_dur.length_mom ();


  /*
    don't do complicated arithmetic with grace notes.
   */
  if (orig
      &&  now_mom().grace_part_ )
    {
      left_to_do_ = Rational (0,0);
    }
  
}
 
void
Completion_heads_engraver::stop_translation_timestep ()
{
  for (int i=0; i < note_p_arr_.size (); i++)
    {
      typeset_grob (note_p_arr_[i]);
    }
  note_p_arr_.clear ();
  
  for (int i=0; i < dot_p_arr_.size (); i++)
    {
      typeset_grob (dot_p_arr_[i]);
    }
  dot_p_arr_.clear ();

  for (int i = scratch_note_reqs_.size(); i--;)
    {
      scm_gc_unprotect_object (scratch_note_reqs_[i]->self_scm () );
      
    }
  scratch_note_reqs_.clear();
}

Tie_req * tie_req = 0;

void
Completion_heads_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();
  if (note_end_mom_.main_part_ <= now.main_part_)
    {
      note_req_l_arr_.clear ();
    }

  if (left_to_do_)
    {
      if (!tie_req)
	tie_req = new Tie_req;
      
      bool succ = daddy_trans_l_->try_music (tie_req);
      if (!succ)
	{
	  programming_error ("Completion_heads_engraver: no-one to make tie.");
	}
    }
}

Completion_heads_engraver::Completion_heads_engraver()
{
}

ENTER_DESCRIPTION(Completion_heads_engraver,
/* descr */       "This engraver replaces
@code{Note_heads_engraver}. It plays some trickery to
break long notes and automatically tie them into the next measure.",
/* creats*/       "NoteHead Dots",
/* acks  */       "",
/* reads */       "centralCPosition measurePosition measureLength",
/* write */       "");
