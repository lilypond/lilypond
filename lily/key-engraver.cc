/*
  key-engraver.cc -- implement Key_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  */

#include "key-item.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "local-key-item.hh"
#include "bar.hh"
#include "timing-translator.hh"
#include "staff-symbol-referencer.hh"
#include "translator-group.hh"
#include "engraver.hh"
#include "musical-pitch.hh"
#include "protected-scm.hh"


/**
  Make the key signature.
 */
class Key_engraver : public Engraver {
  void create_key(bool);
  void read_req (Key_change_req const * r);

public:
  Key_engraver();
  
  VIRTUAL_COPY_CONS(Translator);

  Key_change_req * keyreq_l_;
  Key_item * item_p_;
  Protected_scm old_accs_;
    
protected:
  virtual void do_creation_processing();
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_music();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
  virtual void acknowledge_element (Score_element_info);
};


Key_engraver::Key_engraver ()
{
  keyreq_l_ = 0;
  item_p_ = 0;
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
      item_p_->set_elt_property ("new-accidentals", get_property ("keySignature"));

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
  if (to_boolean (info.elem_l_->get_elt_property ("clef-interface"))) 
    {
      SCM c =  get_property ("createKeyOnClefChange");
      if (to_boolean (c))
	{
	  create_key (false);
	}
    }
  else if (dynamic_cast<Bar *> (info.elem_l_)
	   && gh_pair_p (get_property ("keySignature")))
    {
      create_key (true);
    }

}

void
Key_engraver::do_process_music ()
{
  if (keyreq_l_ || old_accs_ != get_property ("keySignature"))
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

void
Key_engraver::read_req (Key_change_req const * r)
{
  if (r->pitch_alist_ == SCM_UNDEFINED)
    return;

  SCM n = scm_list_copy (r->pitch_alist_);
  SCM accs = SCM_EOL;
  for (SCM s = get_property ("keyAccidentalOrder");
       gh_pair_p (s); s = gh_cdr (s))
    {
      if (gh_pair_p (scm_member (gh_car (s), n)))
	{
	  accs = gh_cons (gh_car (s), accs);
	  n = scm_delete_x (gh_car (s), n);
	}
    }
  for (SCM s = n ; gh_pair_p (s); s = gh_cdr (s))
    if (gh_scm2int (gh_cdar (s)))
      accs = gh_cons (gh_car (s), accs);

  old_accs_ = get_property ("keySignature");
  daddy_trans_l_->set_property ("keySignature", accs);
}

void
Key_engraver::do_post_move_processing ()
{
  keyreq_l_ = 0;
  old_accs_ = get_property ("keySignature");
}

void
Key_engraver::do_creation_processing ()
{
  daddy_trans_l_->set_property ("keySignature", SCM_EOL);
  old_accs_ = SCM_EOL;
}


ADD_THIS_TRANSLATOR (Key_engraver);

