/*
  head-grav.cc -- part of GNU LilyPond

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <ctype.h>
#include <stdio.h>

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
  make (guitar-like) tablature note
 */
class Tab_note_heads_engraver : public Engraver
{
  Link_array<Item> note_p_arr_;
  
  Link_array<Item> dot_p_arr_;
  Link_array<Note_req> note_req_l_arr_;
  Link_array<Text_script_req> tabstring_req_arr_;
public:
  TRANSLATOR_DECLARATIONS(Tab_note_heads_engraver);

protected:
  virtual void start_translation_timestep ();
  virtual bool try_music (Music *req_l) ;
  virtual void process_music ();

  virtual void stop_translation_timestep ();
};


Tab_note_heads_engraver::Tab_note_heads_engraver()
{
}

bool
Tab_note_heads_engraver::try_music (Music *m) 
{
  if (Note_req * n =dynamic_cast <Note_req *> (m))
    {
      note_req_l_arr_.push (n);
      return true;
    }
  else if (Text_script_req * ts = dynamic_cast<Text_script_req*> (m))
    {
      if (m->get_mus_property ("text-type") != ly_symbol2scm ("finger")) return false;
      
      //if (tabstring_req_arr_.size () < note_req_l_arr_.size ()) {
        tabstring_req_arr_.push (ts);
      //}
      return true;
    }
  else if (dynamic_cast<Busy_playing_req*> (m))
    {
      return note_req_l_arr_.size ();
    }
  
  return false;
}


void
Tab_note_heads_engraver::process_music ()
{
  /*
  for (int i=0; i < tabstring_req_arr_.size (); i++) {
      Music * tabstring_req = tabstring_req_arr_[i];
      
      size_t lenp;
      char* tab_string_as_str = gh_scm2newstr(tabstring_req->get_mus_property ("text"), &lenp);
  }
  */
  
  for (int i=0; i < note_req_l_arr_.size (); i++)
    {
      Item * note_p  = new Item (get_property ("TabNoteHead"));
      
      Music * req = note_req_l_arr_[i];
      
      Music * tabstring_req = tabstring_req_arr_[i];
      
      size_t lenp;
      char* tab_string_as_str = gh_scm2newstr(tabstring_req->get_mus_property ("text"), &lenp);
      int tab_string = atoi(tab_string_as_str);
      
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
	  announce_grob (d, SCM_EOL);
	  dot_p_arr_.push (d);
	}
      
      int pos = 2 * tab_string - 2; // No tab-note between the string !!!
      SCM c0 = get_property ("centralCPosition");
      if (gh_number_p (c0)) pos += gh_scm2int (c0);
      
      note_p->set_grob_property ("tab-string", gh_int2scm (tab_string));
      
      note_p->set_grob_property ("staff-position", gh_int2scm (pos));
      announce_grob (note_p, req->self_scm());
      note_p_arr_.push (note_p);
    }
}

void
Tab_note_heads_engraver::stop_translation_timestep ()
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
  
  tabstring_req_arr_.clear ();
}

void
Tab_note_heads_engraver::start_translation_timestep ()
{
}


ENTER_DESCRIPTION(Tab_note_heads_engraver,
/* descr */       "Generate one or more tablature noteheads from Music of type Note_req.",
/* creats*/       "TabNoteHead Dots",
/* acks  */       "",
/* reads */       "centralCPosition",
/* write */       "");
