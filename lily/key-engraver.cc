/*
  key-reg.cc -- implement Key_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  */
#include "key-engraver.hh"
#include "key-item.hh"
#include "command-request.hh"
#include "local-key-engraver.hh"
#include "musical-request.hh"
#include "local-key-item.hh"
#include "bar.hh"
#include "time-description.hh"

Key_engraver::Key_engraver ()
{
  kit_p_ = 0;
  do_post_move_processing ();
}

void
Key_engraver::create_key ()
{
  if (!kit_p_) 
    {
      kit_p_ = new Key_item;
      kit_p_->break_priority_i_ = -1; // ugh
      announce_element (Score_element_info (kit_p_,keyreq_l_));
      kit_p_->set (key_.multi_octave_b_, accidental_idx_arr_, old_accidental_idx_arr_);
    }
}

bool
Key_engraver::do_try_request (Request * req_l)
{
  Command_req* creq_l= req_l->access_Command_req ();
  if (!creq_l|| !creq_l->access_Key_change_req ())
    return false;
   
  if (keyreq_l_)
    return false;		// TODO
  keyreq_l_ = creq_l->access_Key_change_req ();
  read_req (keyreq_l_);
  return true;
}

void
Key_engraver::acknowledge_element (Score_element_info info)
{
  Command_req * r_l = info.req_l_->access_Command_req () ;

  if (r_l && r_l->access_Clef_change_req ()) 
    {
      int i= get_property ("createKeyOnClefChange").length_i ();
      if (i)
	create_key ();
    }
  else if (info.elem_l_->is_type_b (Bar::static_name ())
	   && accidental_idx_arr_.size ()) 
    {
      if (!keyreq_l_)
	default_key_b_ = true;
      create_key ();
    }

}

void
Key_engraver::do_process_requests ()
{
  if (keyreq_l_) 
    {
      create_key ();
    }
}

void
Key_engraver::do_pre_move_processing ()
{ 
  if (kit_p_) 
    {
      kit_p_->default_b_ = default_key_b_;
      typeset_element (kit_p_);
      kit_p_ = 0;
    }
}


/*
  TODO Slightly hairy.  
 */
void
Key_engraver::read_req (Key_change_req const * r)
{
  old_accidental_idx_arr_ = accidental_idx_arr_;
  key_.clear ();
  Scalar prop = get_property ("keyoctaviation");
  if (prop.length_i () > 0)
    {
      key_.multi_octave_b_ = ! prop.to_bool ();
    }
  
  accidental_idx_arr_.clear ();

  if (r->ordinary_key_b_) 
    {
      int p;
      if (r->pitch_arr_.size () < 1) 
        {
	  r->warning (_ ("No key name: assuming `C'"));
	  p = 0;
	}
      else
	{
	  p = r->pitch_arr_[0].semitone_pitch ();
	  if (r->minor_b ())
	    p += 3;
	}
      /* Solve the equation 7*no_of_acc mod 12 = p, -6 <= no_of_acc <= 5 */
      int no_of_acc = (7*p) % 12;
      no_of_acc = (no_of_acc + 18) % 12 -6;

      /* Correct from flats to sharps or vice versa */
      if (no_of_acc * r->pitch_arr_[0].accidental_i_ < 0)
	no_of_acc += 12 * sign (r->pitch_arr_[0].accidental_i_);

      if (no_of_acc < 0) 
	{
	  int accidental = 6 ; // First accidental: bes
	  for ( ; no_of_acc < 0 ; no_of_acc++ ) 
	    {
	      Musical_pitch m;
	      m.accidental_i_ = -1;
	      m.notename_i_ = accidental;
	      if (key_.multi_octave_b_)
		key_.set (m);
	      else
		key_.set (m.notename_i_, m.accidental_i_);
	      accidental_idx_arr_.push (m);
	      
	      accidental = (accidental + 3) % 7 ;
	    }
	}
      else 
	{ 
	  int accidental = 3 ; // First accidental: fis
	  for ( ; no_of_acc > 0 ; no_of_acc-- ) 
	    {
	      Musical_pitch m;
	      m.accidental_i_ = 1;
	      m.notename_i_ = accidental;
	      if (key_.multi_octave_b_)
		key_.set (m);
	      else
		key_.set (m.notename_i_, m.accidental_i_);
	      accidental_idx_arr_.push (m);
	      
	      accidental = (accidental + 4) % 7 ;
	    }
	}
    }
  else // Special key
    {
      for (int i = 0; i < r->pitch_arr_.size (); i ++) 
	{
	  Musical_pitch m_l =r->pitch_arr_[i];
	  if (key_.multi_octave_b_)
	    key_.set (m_l);
	  else
	    key_.set (m_l.notename_i_, m_l.accidental_i_);
	  
	  accidental_idx_arr_.push (m_l);
	}
    }
}

void
Key_engraver::do_post_move_processing ()
{
  keyreq_l_ = 0;
  default_key_b_ = false;
  old_accidental_idx_arr_.clear ();
}


IMPLEMENT_IS_TYPE_B1 (Key_engraver,Engraver);
ADD_THIS_TRANSLATOR (Key_engraver);

