/*
  head-grav.cc -- part of GNU LilyPond

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "note-head.hh"
#include "note-heads-engraver.hh"
#include "paper-def.hh"
#include "musical-request.hh"
#include "dots.hh"
#include "dot-column.hh"

Note_heads_engraver::Note_heads_engraver()
{
}

bool
Note_heads_engraver::do_try_music (Music *m) 
{
  if (Note_req * n =dynamic_cast <Note_req *> (m))
    {
      note_req_l_arr_.push (n);
      notes_end_pq_.insert (now_mom () + m->length_mom ());
      
      return true;
    }
  else if (Tonic_req* t = dynamic_cast<Tonic_req*> (m))
    {
      return true;
    }
  else if (Busy_playing_req * p = dynamic_cast<Busy_playing_req*> (m))
    {
      return notes_end_pq_.size ();
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
Note_heads_engraver::do_process_requests()
{
  if (note_p_arr_.size ())
    return ;
  
  String noteheadstyle = get_property ("noteHeadStyle", 0);
  for (int i=0; i < note_req_l_arr_.size (); i++)
    {
      Note_head *note_p  = new Note_head;
      Note_req * note_req_l = note_req_l_arr_[i];
      note_p->balltype_i_ = note_req_l->duration_.durlog_i_;

      if (note_req_l->duration_.dots_i_)
	{
	  Dots * d = new Dots;
	  note_p->dots_l_ = d;
	  d->dots_i_ = note_req_l->duration_.dots_i_;

	  Scalar dir = get_property ("verticalDirection",0);
	  if (dir.isdir_b())
	    {
	      d->resolve_dir_ = (Direction)(int)dir;
	    }
	  
	  announce_element (Score_element_info (d,0));
	  dot_p_arr_.push (d);
	}
      note_p->position_i_  = note_req_l->pitch_.steps ();

      if (noteheadstyle == "transparent")
	note_p->set_elt_property (transparent_scm_sym, SCM_BOOL_T);
      else 
        note_p->set_elt_property (style_scm_sym,
				  ly_ch_C_to_scm (noteheadstyle.ch_C()));

      
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

void
Note_heads_engraver::do_post_move_processing()
{
  Moment n (now_mom ());
  while (notes_end_pq_.size () && notes_end_pq_.front () <=n)
    notes_end_pq_.get ();
}



ADD_THIS_TRANSLATOR(Note_heads_engraver);
