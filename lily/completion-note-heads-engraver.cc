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
  Link_array<Item> notes_;
  
  Link_array<Item> dots_;
  Link_array<Music> note_reqs_;
  Link_array<Music> scratch_note_reqs_;

  Moment note_end_mom_;
  bool first_b_;
  Rational left_to_do_;
  Rational do_nothing_until_;
  
  Moment next_barline_moment ();
  Duration find_nearest_duration (Rational length);
  
public:
  TRANSLATOR_DECLARATIONS(Completion_heads_engraver);

protected:
  virtual void initialize ();
  virtual void start_translation_timestep ();
  virtual bool try_music (Music *req) ;
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
  if (m->is_mus_type ("note-event"))
    {
      note_reqs_.push (m);

      first_b_ = true;
      Moment musiclen = m->length_mom ();
      Moment now = now_mom();

      if (now_mom ().grace_part_)
	{
	  musiclen.grace_part_ = musiclen.main_part_ ;
	  musiclen.main_part_ = Rational (0,1);
	}
      note_end_mom_  = note_end_mom_ >? (now + musiclen);
      do_nothing_until_ = Rational (0,0);
      
      return true;
    }
  else if  (m->is_mus_type ("busy-playing-event"))
    {
      return note_reqs_.size ();
    }
  
  return false;
  
}

/*
  The duration _until_ the next barline.
 */
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

  Moment now =  now_mom ();
  if (do_nothing_until_ > now.main_part_)
    return ;
  
  Duration note_dur;
  Duration *orig = 0;
  if (left_to_do_)
    {
      note_dur = find_nearest_duration (left_to_do_);
    }
  else
    {
      orig = unsmob_duration (note_reqs_[0]->get_mus_property ("duration"));
      note_dur = *orig;
    }
  Moment nb = next_barline_moment ();
  if (nb < note_dur.length_mom ())
    {
      note_dur = find_nearest_duration (nb.main_part_);

      Moment next = now;
      next.main_part_ += note_dur.length_mom ();
      top_engraver ()->add_moment_to_process (next);
      do_nothing_until_ = next.main_part_;
    }

  if (orig)
    {
      left_to_do_ = orig->length_mom ();
    }

  if (orig && note_dur.length_mom() != orig->length_mom())
    {
      if (!scratch_note_reqs_.size ())
	for (int i = 0; i < note_reqs_.size (); i++)
	  {
	    Music * m = note_reqs_[i]->clone ();
	    scratch_note_reqs_.push (m);
	  }
    }

  
  for (int i = 0;
       left_to_do_ && i < note_reqs_.size (); i++)
    {
      Item *note  = new Item (get_property ("NoteHead"));
      
      Music * req =  note_reqs_[i];
      if (scratch_note_reqs_.size())
	{
	  req = scratch_note_reqs_[i];
	  SCM pits = note_reqs_[i]->get_mus_property ("pitch");
	  req->set_mus_property ("pitch",pits);
	}
      
      req->set_mus_property ("duration", note_dur.smobbed_copy ());
      note->set_grob_property ("duration-log",
				 gh_int2scm (note_dur.duration_log ()));
      
      int dots= note_dur.dot_count ();
      if (dots)
	{
	  Item * d = new Item (get_property ("Dots"));
	  Rhythmic_head::set_dots (note, d);

	  /*
	   measly attempt to save an eeny-weenie bit of memory.
	  */
	  if (dots != gh_scm2int (d->get_grob_property ("dot-count")))
	    d->set_grob_property ("dot-count", gh_int2scm (dots));

	  d->set_parent (note, Y_AXIS);
	  announce_grob (d, SCM_EOL);
	  dots_.push (d);
	}

      Pitch *pit =unsmob_pitch (req->get_mus_property ("pitch"));

      int pos = pit->steps ();
      SCM c0 = get_property ("centralCPosition");
      if (gh_number_p (c0))
	pos += gh_scm2int (c0);

      note->set_grob_property ("staff-position",   gh_int2scm (pos));
      announce_grob (note,req->self_scm ());
      notes_.push (note);
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
  for (int i=0; i < notes_.size (); i++)
    {
      typeset_grob (notes_[i]);
    }
  notes_.clear ();
  
  for (int i=0; i < dots_.size (); i++)
    {
      typeset_grob (dots_[i]);
    }
  dots_.clear ();

  for (int i = scratch_note_reqs_.size(); i--;)
    {
      scm_gc_unprotect_object (scratch_note_reqs_[i]->self_scm () );
      
    }
  scratch_note_reqs_.clear();
}

Music * tie_req = 0;

void
Completion_heads_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();
  if (note_end_mom_.main_part_ <= now.main_part_)
    {
      note_reqs_.clear ();
    }

  if (left_to_do_)
    {
      if (!tie_req)
	tie_req = make_music_by_name (ly_symbol2scm ("TieEvent"));
      
      bool succ = daddy_trans_->try_music (tie_req);
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
/* accepts */     "general-music",
/* acks  */      "",
/* reads */       "centralCPosition measurePosition measureLength",
/* write */       "");
