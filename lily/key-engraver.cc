/*
  key-engraver.cc -- implement Key_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  */

#include "key-item.hh"
#include "command-request.hh"
#include "musical-request.hh"
#include "item.hh"
#include "bar-line.hh"
#include "staff-symbol-referencer.hh"
#include "translator-group.hh"
#include "engraver.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "clef.hh"

/**
  Make the key signature.
 */
class Key_engraver : public Engraver
{
  void create_key (bool);
  void read_req (Key_change_req const * r);
  Key_change_req * keyreq_l_;
  Item * item_p_;

public:
  TRANSLATOR_DECLARATIONS(Key_engraver);

protected:
  virtual void initialize ();
  virtual void finalize ();
  virtual bool try_music (Music *req_l);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void create_grobs ();
  virtual void acknowledge_grob (Grob_info);
};


void
Key_engraver::finalize ()
{
}


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
      item_p_ = new Item (get_property ("KeySignature"));

      item_p_->set_grob_property ("c0-position",
				  get_property ("centralCPosition"));
      
      // todo: put this in basic props.
      item_p_->set_grob_property ("old-accidentals", get_property ("lastKeySignature"));
      item_p_->set_grob_property ("new-accidentals", get_property ("keySignature"));

      announce_grob(item_p_, keyreq_l_ ? keyreq_l_->self_scm() : SCM_EOL);
    }

  if (!def)
    {
      SCM vis = get_property ("explicitKeySignatureVisibility"); 
      if (gh_procedure_p (vis))
	item_p_->set_grob_property ("visibility-lambda",vis);
    }
}      


bool
Key_engraver::try_music (Music * req_l)
{
  if (Key_change_req *kc = dynamic_cast <Key_change_req *> (req_l))
    {
      if (keyreq_l_ && !keyreq_l_->equal_b (kc))
	{
	  kc->origin ()->warning (_ ("Conflicting key signatures found."));
	  keyreq_l_->origin ()->warning (_ ("This was the other key definition."));	  
	  return false;
	}
      keyreq_l_ = kc;
      read_req (keyreq_l_);

      return true;
    }   
  return  false;
}


void
Key_engraver::acknowledge_grob (Grob_info info)
{
  if (Clef::has_interface (info.grob_l_))
    {
      SCM c =  get_property ("createKeyOnClefChange");
      if (to_boolean (c))
	{
	  create_key (false);
	}
    }
  else if (Bar_line::has_interface (info.grob_l_)
	   && gh_pair_p (get_property ("keySignature")))
    {
      create_key (true);
    }
}


void
Key_engraver::create_grobs ()
{
  if (keyreq_l_ ||
      get_property ("lastKeySignature") != get_property ("keySignature"))
    create_key (false);
}


void
Key_engraver::stop_translation_timestep ()
{
  if (item_p_) 
    {
      typeset_grob (item_p_);
      item_p_ = 0;
    }
}


void
Key_engraver::read_req (Key_change_req const * r)
{
  SCM p = r->get_mus_property ("pitch-alist");
  if (!gh_pair_p (p))
    return;

  SCM n = scm_list_copy (p);
  SCM accs = SCM_EOL;
  for (SCM s = get_property ("keyAccidentalOrder");
       gh_pair_p (s); s = ly_cdr (s))
    {
      if (gh_pair_p (scm_member (ly_car (s), n)))
	{
	  accs = gh_cons (ly_car (s), accs);
	  n = scm_delete_x (ly_car (s), n);
	}
    }
  
  for (SCM s = n ; gh_pair_p (s); s = ly_cdr (s))
    if (gh_scm2int (ly_cdar (s)))
      accs = gh_cons (ly_car (s), accs);

  daddy_trans_l_->set_property ("lastKeySignature",
				get_property ("keySignature"));
  daddy_trans_l_->set_property ("keySignature", accs);
}


void
Key_engraver::start_translation_timestep ()
{
  keyreq_l_ = 0;
  daddy_trans_l_->set_property ("lastKeySignature", get_property ("keySignature"));
}


void
Key_engraver::initialize ()
{
  daddy_trans_l_->set_property ("keySignature", SCM_EOL);
  daddy_trans_l_->set_property ("lastKeySignature", SCM_EOL);
}


ENTER_DESCRIPTION(Key_engraver,
/* descr */       "",
/* creats*/       "KeySignature",
/* acks  */       "bar-line-interface clef-interface",
/* reads */       "keySignature lastKeySignature explicitKeySignatureVisibility createKeyOnClefChange keyAccidentalOrder keySignature",
/* write */       "lastKeySignature");
