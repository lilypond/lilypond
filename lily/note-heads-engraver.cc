/*
  head-grav.cc -- part of GNU LilyPond

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

/**
  make balls and rests
 */
class Note_heads_engraver : public Engraver
{
  Link_array<Item> note_p_arr_;
  
  Link_array<Item> dot_p_arr_;
  Link_array<Note_req> note_req_l_arr_;

public:
  TRANSLATOR_DECLARATIONS(Note_heads_engraver);

protected:
  virtual void start_translation_timestep ();
  virtual bool try_music (Music *req_l) ;
  virtual void process_music ();

  virtual void stop_translation_timestep ();
};


Note_heads_engraver::Note_heads_engraver()
{
}

bool
Note_heads_engraver::try_music (Music *m) 
{
  if (Note_req * n =dynamic_cast <Note_req *> (m))
    {
      note_req_l_arr_.push (n);
      return true;
    }
  else if (dynamic_cast<Busy_playing_req*> (m))
    {
      return note_req_l_arr_.size ();
    }
  
  return false;
}


void
Note_heads_engraver::process_music ()
{
  for (int i=0; i < note_req_l_arr_.size (); i++)
    {
      Item *note_p  = new Item (get_property ("NoteHead"));
      
      Staff_symbol_referencer::set_interface (note_p);
      
      Music * req = note_req_l_arr_[i];
      
      Duration dur = *unsmob_duration (req->get_mus_property ("duration"));

      note_p->set_grob_property ("duration-log", gh_int2scm (dur.duration_log ()));

      if (dur.dot_count ())
	{
	  Item * d = new Item (get_property ("Dots"));
	  Rhythmic_head::set_dots (note_p, d);
	  
	  if (dur.dot_count ()
	      != gh_scm2int (d->get_grob_property ("dot-count")))
	    d->set_grob_property ("dot-count", gh_int2scm (dur.dot_count ()));

	  d->set_parent (note_p, Y_AXIS);
	  announce_grob (d,0);
	  dot_p_arr_.push (d);
	}

      Pitch *pit =unsmob_pitch (req->get_mus_property ("pitch"));

      int pos = pit->steps ();
      SCM c0 = get_property ("centralCPosition");
      if (gh_number_p (c0))
	pos += gh_scm2int (c0);

      note_p->set_grob_property ("staff-position",   gh_int2scm (pos));
      if (to_boolean (get_property ("easyPlay")))
	{
	  char s[2] = "a";
	  s[0] = (pit->notename_i_ + 2)%7 + 'a';

	  s[0] = toupper (s[0]);
	  note_p->set_grob_property ("note-character", ly_str02scm (s));
	}
      
      announce_grob (note_p,req);
      note_p_arr_.push (note_p);
    }
}

void
Note_heads_engraver::stop_translation_timestep ()
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
  
  note_req_l_arr_.clear ();
}

void
Note_heads_engraver::start_translation_timestep ()
{
}


ENTER_DESCRIPTION(Note_heads_engraver,
/* descr */       "Generate one or more noteheads from Music of type Note_req.",
/* creats*/       "NoteHead Dots",
/* acks  */       "",
/* reads */       "easyPlay centralCPosition",
/* write */       "");
