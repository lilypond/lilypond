/*   
  note-name-engraver.cc --  implement Note_name_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "musical-request.hh"
#include "text-item.hh"

class Note_name_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  Link_array<Note_req> req_l_arr_;
  Link_array<Text_item> texts_;
  virtual bool  do_try_music (Music*m);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();
};

bool
Note_name_engraver::do_try_music (Music *m)
{
  if (Note_req *r = dynamic_cast<Note_req* > (m))
    {
      req_l_arr_.push (r);
      return true;
    }
  return false;
}


void
Note_name_engraver::do_process_music ()
{
  String s ;
  for (int i=0; i < req_l_arr_.size (); i++)
    {
      if (i)
	s += " ";
      s += req_l_arr_[i]->pitch_.str ();
    }
  if (s.length_i())
    {
      Text_item * t = new Text_item;
      t->set_elt_property ("text", ly_str02scm ( s.ch_C()));
      announce_element (Score_element_info (t, req_l_arr_[0]));
      texts_.push (t);
    }
}

void
Note_name_engraver::do_pre_move_processing ()
{
  for (int i=0; i < texts_.size (); i++)
    {
      typeset_element (texts_[i]);
    }
  texts_.clear() ;
  req_l_arr_.clear ();
}

ADD_THIS_TRANSLATOR(Note_name_engraver);
