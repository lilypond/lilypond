/*   
  note-name-engraver.cc --  implement Note_name_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "event.hh"
#include "item.hh"

class Note_name_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Note_name_engraver);

  Link_array<Music> reqs_;
  Link_array<Item> texts_;
  virtual bool  try_music (Music*m);
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();
};

bool
Note_name_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("note-event"))
    {
      reqs_.push (m);
      return true;
    }
  return false;
}

void
Note_name_engraver::process_acknowledged_grobs ()
{
  if (texts_.size ())
    return;
  String s ;
  for (int i=0; i < reqs_.size (); i++)
    {
      if (i)
	s += " ";
      s += unsmob_pitch (reqs_[i]->get_mus_property ("pitch"))->string ();
    }
  if (s.length ())
    {
      Item * t = new Item (get_property ("NoteName"));
      t->set_grob_property ("text", scm_makfrom0str (s.to_str0 ()));
      announce_grob(t, reqs_[0]->self_scm());
      texts_.push (t);
    }
}

void
Note_name_engraver::stop_translation_timestep ()
{
  for (int i=0; i < texts_.size (); i++)
    {
      typeset_grob (texts_[i]);
    }
  texts_.clear () ;
  reqs_.clear ();
}


Note_name_engraver::Note_name_engraver()
{
}

ENTER_DESCRIPTION(Note_name_engraver,
/* descr */       "",
/* creats*/       "NoteName",
/* accepts */     "note-event",
/* acks  */      "",
/* reads */       "",
/* write */       "");
