/*
  key-engraver.cc -- implement Key_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  item_p_ = 0;
  do_post_move_processing ();
}

bool
Key_engraver::key_changed_b () const
{
  return keyreq_l_ ;
}

void
Key_engraver::create_key ()
{
  if (!item_p_) 
    {
      item_p_ = new Key_item;
      item_p_->set_elt_property (break_priority_scm_sym, gh_int2scm(-1)); // ugh
      item_p_->multi_octave_b_ = key_.multi_octave_b_;
      announce_element (Score_element_info (item_p_,keyreq_l_));
      

      for (int i = 0; i < accidental_idx_arr_.size(); i++) 
	{
	  Musical_pitch m_l =accidental_idx_arr_[i];
	  int a =m_l.accidental_i_;      
	  if (key_.multi_octave_b_)
	    item_p_->add (m_l.steps (), a);
	  else
	    item_p_->add (m_l.notename_i_, a);
	}

      for (int i = 0 ; i< old_accidental_idx_arr_.size(); i++) 
	{
	  Musical_pitch m_l =old_accidental_idx_arr_[i];
	  int a =m_l.accidental_i_;
	  if (key_.multi_octave_b_)
	    item_p_->add_old (m_l.steps  (), a);
	  else
	    item_p_->add_old (m_l.notename_i_, a);
	}
    }
}      


bool
Key_engraver::do_try_music (Music * req_l)
{
  if (Key_change_req *kc = dynamic_cast <Key_change_req *> (req_l))
    {
      if (keyreq_l_)
	warning ("Fixme: key change merge.");
      keyreq_l_ = kc;
      read_req (keyreq_l_);
      return true;
    }   
  return  false;
}

void
Key_engraver::acknowledge_element (Score_element_info info)
{
  if (dynamic_cast <Clef_change_req *> (info.req_l_)) 
    {
      int i= get_property ("createKeyOnClefChange", 0).length_i ();
      if (i)
	create_key ();
    }
  else if (dynamic_cast<Bar *> (info.elem_l_)
	   && accidental_idx_arr_.size ()) 
    {
      bool def =  (!item_p_);
      create_key ();
      if (def)
	{
	  item_p_->set_elt_property (visibility_lambda_scm_sym,
				    gh_eval_str ("postbreak_only_visibility"));
	}
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
  if (item_p_) 
    {
      typeset_element (item_p_);
      item_p_ = 0;
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
  Scalar prop = get_property ("keyoctaviation", 0);
  if (prop.length_i () > 0)
    {
      key_.multi_octave_b_ = ! prop.to_bool ();
    }
  
  accidental_idx_arr_.clear ();

  if (r->key_.ordinary_key_b_) 
    {
      int no_of_acc = r->key_.ordinary_accidentals_i ();

      // Hmm, can't these be handled/constructed by Key_change_req?
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
      for (int i = 0; i < r->key_.pitch_arr_.size (); i ++) 
	{
	  Musical_pitch m_l =r->key_.pitch_arr_[i];
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
  old_accidental_idx_arr_.clear ();
}



ADD_THIS_TRANSLATOR (Key_engraver);

