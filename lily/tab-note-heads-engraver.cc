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
  Link_array<Item> notes_;
  
  Link_array<Item> dots_;
  Link_array<Note_req> note_reqs_;
  Link_array<Text_script_req> tabstring_reqs_;
public:
  TRANSLATOR_DECLARATIONS(Tab_note_heads_engraver);

protected:
  virtual void start_translation_timestep ();
  virtual bool try_music (Music *req) ;
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
      note_reqs_.push (n);
      return true;
    }
  else if (Text_script_req * ts = dynamic_cast<Text_script_req*> (m))
    {
      if (m->get_mus_property ("text-type") != ly_symbol2scm ("finger")) return false;
      
      while(tabstring_reqs_.size () < note_reqs_.size ()-1)
	tabstring_reqs_.push(0);
      tabstring_reqs_.push(ts);
      return true;
    }
  else if (dynamic_cast<Busy_playing_req*> (m))
    {
      return note_reqs_.size ();
    }
  
  return false;
}


void
Tab_note_heads_engraver::process_music ()
{
  /*
  for (int i=0; i < tabstring_reqs_.size (); i++) {
      Music * tabstring_req = tabstring_reqs_[i];
      
      size_t lenp;
      char* tab_string_as_string = gh_scm2newstr(tabstring_req->get_mus_property ("text"), &lenp);
  }
  */
  
  for (int i=0; i < note_reqs_.size (); i++)
    {
      SCM stringTunings = get_property ("stringTunings");
      int number_of_strings = ((int) gh_length(stringTunings));

      Item * note  = new Item (get_property ("TabNoteHead"));
      
      Music * req = note_reqs_[i];
      
      Music * tabstring_req = 0;
      if(tabstring_reqs_.size()>i)
	tabstring_req = tabstring_reqs_[i];
      // printf("%d %d\n",tabstring_reqs_.size(),i);
      size_t lenp;
      int tab_string;
      bool string_found;
      if (tabstring_req) {
	char* tab_string_as_string = gh_scm2newstr(tabstring_req->get_mus_property ("text"), &lenp);
	tab_string = atoi(tab_string_as_string);
	string_found = true;
      }
      else {
	tab_string = number_of_strings;
	string_found = false;
      }
      
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
      
      
      SCM scm_pitch = req->get_mus_property ("pitch");
      SCM proc      = get_property ("tablatureFormat");
      SCM min_fret_scm = get_property ("minimumFret");
      int min_fret = gh_number_p(min_fret_scm) ? gh_scm2int(min_fret_scm) : 0;

      while(!string_found) {
	int fret = unsmob_pitch(scm_pitch)->semitone_pitch()
	  - gh_scm2int(gh_list_ref(stringTunings,gh_int2scm(tab_string-1)));
	if(fret<min_fret)
	  tab_string--;
	else
	  string_found = true;
      }

      SCM text = gh_call3 (proc, gh_int2scm (tab_string), stringTunings, scm_pitch);

      int pos = 2 * tab_string - 2; // No tab-note between the string !!!
      
      if (number_of_strings % 2) { // odd number of string
	pos++;
      }

      
      note->set_grob_property ("text", text);
      SCM c0 = get_property ("centralCPosition");
      if (gh_number_p (c0)) pos += gh_scm2int (c0);
      
      
      note->set_grob_property ("staff-position", gh_int2scm (pos));
      announce_grob (note, req->self_scm());
      notes_.push (note);
    }
}

void
Tab_note_heads_engraver::stop_translation_timestep ()
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
  
  tabstring_reqs_.clear ();
}

void
Tab_note_heads_engraver::start_translation_timestep ()
{
}


ENTER_DESCRIPTION(Tab_note_heads_engraver,
/* descr */       "Generate one or more tablature noteheads from Music of type Note_req.",
/* creats*/       "TabNoteHead Dots",
/* acks  */       "",
/* reads */       "centralCPosition stringTunings minimumFret",
/* write */       "");

