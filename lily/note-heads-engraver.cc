/*
  head-grav.cc -- part of GNU LilyPond

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "note-head.hh"
#include "note-heads-engraver.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "dots.hh"
#include "dot-column.hh"
#include "staff-symbol-referencer.hh"

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
      Note_head *note_p  = new Note_head;
      
      Staff_symbol_referencer_interface si (note_p);
      si.set_interface ();

      
      Note_req * note_req_l = note_req_l_arr_[i];
      
      note_p->set_elt_property ("duration-log",
				gh_int2scm (note_req_l->duration_.durlog_i_ <? 2));

      if (note_req_l->duration_.dots_i_)
	{
	  Dots * d = new Dots;

	  Staff_symbol_referencer_interface sd (d);
	  sd.set_interface ();
	  
	  note_p->add_dots (d);
	  d->set_elt_property ("dot-count", gh_int2scm (note_req_l->duration_.dots_i_));
	  announce_element (Score_element_info (d,0));
	  dot_p_arr_.push (d);
	}
      si.set_position(note_req_l->pitch_.steps ());

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

