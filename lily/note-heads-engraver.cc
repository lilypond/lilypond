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
#include "engraver.hh"
#include "warn.hh"

/**
  make balls and rests
 */
class Note_heads_engraver : public Engraver
{
  Link_array<Item> notes_;
  
  Link_array<Item> dots_;
  Link_array<Note_req> note_reqs_;

public:
  TRANSLATOR_DECLARATIONS(Note_heads_engraver);

protected:
  virtual void start_translation_timestep ();
  virtual bool try_music (Music *req) ;
  virtual void process_music ();

  virtual void stop_translation_timestep ();

private:
  bool in_ligature;
};

Note_heads_engraver::Note_heads_engraver()
{
  in_ligature = 0;
}

bool
Note_heads_engraver::try_music (Music *m) 
{
  if (Note_req * n =dynamic_cast <Note_req *> (m))
    {
      note_reqs_.push (n);
      return true;
    }
  else if (dynamic_cast<Busy_playing_req*> (m))
    {
      return note_reqs_.size ();
    }
  else if (Span_req *req_ = dynamic_cast<Span_req*> (m))
    {
      if (scm_equal_p (req_->get_mus_property ("span-type"),
		       scm_makfrom0str ("abort")) == SCM_BOOL_T)
	{
	  in_ligature = 0;
	}
      else if (scm_equal_p (req_->get_mus_property ("span-type"),
			    scm_makfrom0str ("ligature")) == SCM_BOOL_T)
	{
	  in_ligature = (req_->get_span_dir () == START);
	}
    }
  
  return false;
}


void
Note_heads_engraver::process_music ()
{
  for (int i=0; i < note_reqs_.size (); i++)
    {
      Item *note =
	new Item (get_property ((in_ligature) ? "LigatureHead" : "NoteHead"));

      Music * req = note_reqs_[i];
      
      Duration dur = *unsmob_duration (req->get_mus_property ("duration"));

      note->set_grob_property ("duration-log", gh_int2scm (dur.duration_log ()));

      if (dur.dot_count ())
	{
	  Item * d = new Item (get_property ("Dots"));
	  Rhythmic_head::set_dots (note, d);
	  
	  if (dur.dot_count ()
	      != gh_scm2int (d->get_grob_property ("dot-count")))
	    d->set_grob_property ("dot-count", gh_int2scm (dur.dot_count ()));

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
      announce_grob (note,req->self_scm());
      notes_.push (note);
    }
}

void
Note_heads_engraver::stop_translation_timestep ()
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
  
  note_reqs_.clear ();
}

void
Note_heads_engraver::start_translation_timestep ()
{
}


ENTER_DESCRIPTION(Note_heads_engraver,
/* descr */       "Generate one or more noteheads from Music of type Note_req.",
/* creats*/       "NoteHead Dots",
/* accepts */     "general-music",
/* acks  */      "",
/* reads */       "centralCPosition",
/* write */       "");
