/*   
  note-name-engraver.cc --  implement Note_name_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "musical-request.hh"
#include "item.hh"

class Note_name_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Note_name_engraver);

  Link_array<Note_req> req_l_arr_;
  Link_array<Item> texts_;
  virtual bool  try_music (Music*m);
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();
};

bool
Note_name_engraver::try_music (Music *m)
{
  if (Note_req *r = dynamic_cast<Note_req* > (m))
    {
      req_l_arr_.push (r);
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
  for (int i=0; i < req_l_arr_.size (); i++)
    {
      if (i)
	s += " ";
      s += unsmob_pitch (req_l_arr_[i]->get_mus_property ("pitch"))->str ();
    }
  if (s.length_i ())
    {
      Item * t = new Item (get_property ("NoteName"));
      t->set_grob_property ("text", ly_str02scm (s.ch_C ()));
      announce_grob(t, req_l_arr_[0]->self_scm());
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
  req_l_arr_.clear ();
}


Note_name_engraver::Note_name_engraver()
{
}

ENTER_DESCRIPTION(Note_name_engraver,
/* descr */       "",
/* creats*/       "NoteName",
/* acks  */       "",
/* reads */       "",
/* write */       "");
