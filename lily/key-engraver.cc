/*
  key-engraver.cc -- implement Key_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  */

#include "key-engraver.hh"
#include "key-item.hh"
#include "command-request.hh"
#include "local-key-engraver.hh"
#include "musical-request.hh"
#include "local-key-item.hh"
#include "bar.hh"
#include "timing-translator.hh"
#include "staff-symbol-referencer.hh"

/*
  this is a large mess. Please clean this to use Basic properties and
  Scheme data structs.
 */

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
Key_engraver::create_key (bool def)
{
  if (!item_p_) 
    {
      item_p_ = new Key_item ( get_property ("basicKeyProperties"));
      
      item_p_->set_elt_property ("c0-position", gh_int2scm (0));

      // todo: put this in basic props.
      item_p_->set_elt_property ("old-accidentals", old_accs_);
      item_p_->set_elt_property ("new-accidentals", new_accs_);

      Staff_symbol_referencer_interface st (item_p_);
      st.set_interface ();

      SCM prop = get_property ("keyOctaviation");
      bool multi = to_boolean (prop);
      
      if (multi)
	item_p_->set_elt_property ("multi-octave", gh_bool2scm (multi));
      
      announce_element (Score_element_info (item_p_,keyreq_l_));
    }

  if (!def)
    item_p_->set_elt_property ("visibility-lambda",
			       scm_eval (ly_symbol2scm  ("all-visible")));

}      


bool
Key_engraver::do_try_music (Music * req_l)
{
  if (Key_change_req *kc = dynamic_cast <Key_change_req *> (req_l))
    {
      if (keyreq_l_)
	warning (_ ("FIXME: key change merge"));
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
      SCM c =  get_property ("createKeyOnClefChange");
      if (to_boolean (c))
	{
	  create_key (false);
      
	}
    }
  else if (dynamic_cast<Bar *> (info.elem_l_)
	   && gh_pair_p (new_accs_))
    {
      create_key (true);
    }

}

void
Key_engraver::do_process_music ()
{
  if (keyreq_l_) 
    {
      create_key (false);
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
  TODO.  Use properties; and this is too hairy.
 */
void
Key_engraver::read_req (Key_change_req const * r)
{
  if (!r->key_)
    return;
  
  key_.clear ();
  SCM prop = get_property ("keyOctaviation");
  bool multi = to_boolean (prop);

  SCM n = SCM_EOL;

  if (r->key_->ordinary_key_b_) 
    {
      int no_of_acc = r->key_->ordinary_accidentals_i ();

      // Hmm, can't these be handled/constructed by Key_change_req?
      if (no_of_acc < 0) 
	{
	  int accidental = 6 ; // First accidental: bes
	  for ( ; no_of_acc < 0 ; no_of_acc++ ) 
	    {
	      Musical_pitch m;
	      m.accidental_i_ = -1;
	      m.notename_i_ = accidental;
	      if (multi)
		key_.set (m.octave_i_, m.notename_i_, m.accidental_i_);
	      else
		key_.set (m.notename_i_, m.accidental_i_);

	      SCM pair = gh_cons (gh_int2scm (m.notename_i_),
				  gh_int2scm (m.accidental_i_));
	      n = gh_cons (pair, n) ;
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
	      if (multi)
		key_.set (m.octave_i_, m.notename_i_, m.accidental_i_);
	      else
		key_.set (m.notename_i_, m.accidental_i_);

	      SCM pair = gh_cons (gh_int2scm (m.notename_i_),
				  gh_int2scm (m.accidental_i_));
	      n = gh_cons (pair, n);
	      
	      accidental = (accidental + 4) % 7 ;
	    }
	}
    }
  else // Special key
    {
      for (int i = 0; i < r->key_->pitch_arr_.size (); i ++) 
	{
	  Musical_pitch m_l =r->key_->pitch_arr_[i];
	  if (multi)
	    key_.set (m_l.octave_i_, m_l.notename_i_, m_l.accidental_i_);
	  else
	    key_.set (m_l.notename_i_, m_l.accidental_i_);

	  SCM pair = gh_cons (gh_int2scm (m_l.notename_i_),
			      gh_int2scm (m_l.accidental_i_));
	  n = gh_cons (pair, n);
	}
    }

  old_accs_ = new_accs_;
  new_accs_ = n;
  
}

void
Key_engraver::do_post_move_processing ()
{
  keyreq_l_ = 0;
  old_accs_ = SCM_EOL;
}

ADD_THIS_TRANSLATOR (Key_engraver);

