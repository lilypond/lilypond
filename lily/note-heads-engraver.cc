/*
  head-grav.cc -- part of GNU LilyPond

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "rhythmic-head.hh"
#include "engraver.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "dots.hh"
#include "dot-column.hh"
#include "staff-symbol-referencer.hh"
#include "engraver.hh"
#include "pqueue.hh"
#include "item.hh"

/**
  make balls and rests
 */
class Note_heads_engraver : public Engraver
{
  Link_array<Item> note_p_arr_;
  Link_array<Item> dot_p_arr_;
  Link_array<Note_req> note_req_l_arr_;
  Moment note_end_mom_;
public:
  VIRTUAL_COPY_CONS(Translator);
  Note_heads_engraver();
  
protected:
  virtual bool do_try_music (Music *req_l) ;
  virtual void do_process_music();
  virtual void do_pre_move_processing();
};





Note_heads_engraver::Note_heads_engraver()
{
}

bool
Note_heads_engraver::do_try_music (Music *m) 
{
  if (Note_req * n =dynamic_cast <Note_req *> (m))
    {
      note_req_l_arr_.push (n);
      note_end_mom_  = note_end_mom_ >? now_mom () + m->length_mom ();
      
      return true;
    }
  else if (Tonic_req* t = dynamic_cast<Tonic_req*> (m))
    {
      return true;
    }
  else if (Inversion_req* i = dynamic_cast<Inversion_req*> (m))
    {
      return true;
    }
  else if (Bass_req* b = dynamic_cast<Bass_req*> (m))
    {
      return true;
    }
  else if (Busy_playing_req * p = dynamic_cast<Busy_playing_req*> (m))
    {
      return now_mom () < note_end_mom_;
    }
  else if (Pitch_interrogate_req *p = dynamic_cast<Pitch_interrogate_req*> (m))
    {
      for (int i= note_req_l_arr_.size (); i--;)
	p->pitch_arr_.push (note_req_l_arr_[i]->pitch_); // GUH UGH UGHUGH.
      return true;
    }
  return false;
  
}

void
Note_heads_engraver::do_process_music()
{
  if (note_p_arr_.size ())
    return ;
  
  for (int i=0; i < note_req_l_arr_.size (); i++)
    {
      Item *note_p  = new Item (get_property ("basicNoteHeadProperties"));
      
      Staff_symbol_referencer::set_interface (note_p);


      
      Note_req * note_req_l = note_req_l_arr_[i];
      
      note_p->set_elt_property ("duration-log",
				gh_int2scm (note_req_l->duration_.durlog_i_ <? 2));

      if (note_req_l->duration_.dots_i_)
	{
	  Item * d = new Item (get_property ("basicDotsProperties"));

	  Staff_symbol_referencer::set_interface (d);
	  
	  Rhythmic_head::set_dots (note_p, d);
	  
	  if (note_req_l->duration_.dots_i_
	      != gh_scm2int (d->get_elt_property ("dot-count")))
	    d->set_elt_property ("dot-count", gh_int2scm (note_req_l->duration_.dots_i_));

	  d->set_parent (note_p, Y_AXIS);
	  d->add_offset_callback (Dots::quantised_position_callback, Y_AXIS);
	  announce_element (Score_element_info (d,0));
	  dot_p_arr_.push (d);
	}

      note_p->set_elt_property("staff-position",  gh_int2scm (note_req_l->pitch_.steps ()));

      Score_element_info itinf (note_p,note_req_l);
      announce_element (itinf);
      note_p_arr_.push (note_p);
    }
}
 
void
Note_heads_engraver::do_pre_move_processing()
{
  for (int i=0; i < note_p_arr_.size (); i++)
    {
      typeset_element (note_p_arr_[i]);
    }
  note_p_arr_.clear ();
  for (int i=0; i < dot_p_arr_.size (); i++)
    {
      typeset_element (dot_p_arr_[i]);
    }
  dot_p_arr_.clear ();
  
  note_req_l_arr_.clear ();
}




ADD_THIS_TRANSLATOR(Note_heads_engraver);

