/*
  key-engraver.cc -- implement Key_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  */



#include "event.hh"
#include "item.hh"
#include "bar-line.hh"
#include "staff-symbol-referencer.hh"
#include "translator-group.hh"
#include "engraver.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "clef.hh"

/*
  TODO: The representation  of key sigs is all fucked.
 */

/**
  Make the key signature.
 */
class Key_engraver : public Engraver
{
  void create_key (bool);
  void read_ev (Key_change_ev const * r);
  Key_change_ev * key_ev_;
  Item * item_;

public:
  TRANSLATOR_DECLARATIONS(Key_engraver);

protected:
  virtual void initialize ();
  virtual void finalize ();
  virtual bool try_music (Music *ev);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
};


void
Key_engraver::finalize ()
{
}


Key_engraver::Key_engraver ()
{
  key_ev_ = 0;
  item_ = 0;
}


void
Key_engraver::create_key (bool def)
{
  if (!item_) 
    {
      item_ = new Item (get_property ("KeySignature"));

      item_->set_grob_property ("c0-position",
				get_property ("centralCPosition"));
      
      // todo: put this in basic props.
      item_->set_grob_property ("old-accidentals", get_property ("lastKeySignature"));
      item_->set_grob_property ("new-accidentals", get_property ("keySignature"));

      announce_grob(item_, key_ev_ ? key_ev_->self_scm() : SCM_EOL);
    }

  if (!def)
    {
      SCM vis = get_property ("explicitKeySignatureVisibility"); 
      if (gh_procedure_p (vis))
	item_->set_grob_property ("break-visibility",vis);
    }
}      


bool
Key_engraver::try_music (Music * ev)
{
  //  if (Key_change_ev *kc = dynamic_cast <Key_change_ev *> (ev))
  if (ev->is_mus_type ("key-change-event"))
    {
      if (!key_ev_)
	{
	  /*
	    do this only once, just to be on the safe side.
	    */	    
	  key_ev_ = dynamic_cast<Key_change_ev*> (ev); // UGH.
	  read_ev (key_ev_);
	}
      
      return true;
    }   
  return  false;
}


void
Key_engraver::acknowledge_grob (Grob_info info)
{
  if (Clef::has_interface (info.grob_))
    {
      SCM c =  get_property ("createKeyOnClefChange");
      if (to_boolean (c))
	{
	  create_key (false);
	}
    }
  else if (Bar_line::has_interface (info.grob_)
	   && gh_pair_p (get_property ("keySignature")))
    {
      create_key (true);
    }
}


void
Key_engraver::process_music ()
{
  if (key_ev_ ||
      get_property ("lastKeySignature") != get_property ("keySignature"))
    create_key (false);
}


void
Key_engraver::stop_translation_timestep ()
{
  if (item_) 
    {
      typeset_grob (item_);
      item_ = 0;
    }
}


void
Key_engraver::read_ev (Key_change_ev const * r)
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

  daddy_trans_->set_property ("keySignature", accs);
  daddy_trans_->set_property ("tonic" ,
			      r->get_mus_property ("tonic"));
}


void
Key_engraver::start_translation_timestep ()
{
  key_ev_ = 0;
  daddy_trans_->set_property ("lastKeySignature", get_property ("keySignature"));
}


void
Key_engraver::initialize ()
{
  daddy_trans_->set_property ("keySignature", SCM_EOL);
  daddy_trans_->set_property ("lastKeySignature", SCM_EOL);

  Pitch p(0,0,0);
  daddy_trans_->set_property ("tonic", p.smobbed_copy ());

}


ENTER_DESCRIPTION(Key_engraver,
/* descr */       "",
/* creats*/       "KeySignature",
/* accepts */     "key-change-event",
/* acks  */      "bar-line-interface clef-interface",
/* reads */       "keySignature lastKeySignature explicitKeySignatureVisibility createKeyOnClefChange keyAccidentalOrder keySignature",
/* write */       "lastKeySignature tonic keySignature");
