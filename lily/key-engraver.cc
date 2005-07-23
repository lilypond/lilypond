/*
  key-engraver.cc -- implement Key_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "item.hh"
#include "bar-line.hh"
#include "staff-symbol-referencer.hh"
#include "context.hh"
#include "engraver.hh"
#include "protected-scm.hh"
#include "clef.hh"
#include "pitch.hh"

/*
  TODO: The representation  of key sigs is all fucked.
*/

/**
   Make the key signature.
*/
class Key_engraver : public Engraver
{
  void create_key (bool);
  void read_ev (Music const *r);

  Music *key_ev_;
  Item *item_;
  Item *cancellation_;
public:
  TRANSLATOR_DECLARATIONS (Key_engraver);

protected:
  virtual void initialize ();
  virtual void finalize ();
  virtual bool try_music (Music *ev);
  PRECOMPUTED_VIRTUAL void stop_translation_timestep ();
  PRECOMPUTED_VIRTUAL void process_music ();

  DECLARE_ACKNOWLEDGER(clef);
  DECLARE_ACKNOWLEDGER(bar_line);
  
};

void
Key_engraver::finalize ()
{
}

Key_engraver::Key_engraver ()
{
  key_ev_ = 0;
  item_ = 0;
  cancellation_ = 0;
}

void
Key_engraver::create_key (bool def)
{
  if (!item_)
    {
      item_ = make_item ("KeySignature", key_ev_ ? key_ev_->self_scm () : SCM_EOL);

      item_->set_property ("c0-position",
			   get_property ("middleCPosition"));

      SCM last = get_property ("lastKeySignature");
      SCM key = get_property ("keySignature");
      if (to_boolean (get_property ("printKeyCancellation"))
	  && !scm_is_eq (last, key))
	{
	  cancellation_ = make_item ("KeyCancellation", key_ev_ ? key_ev_->self_scm () : SCM_EOL);
	  cancellation_->set_property ("old-accidentals", last);
	  cancellation_->set_property ("c0-position",
				       get_property ("middleCPosition"));
	}
      item_->set_property ("new-accidentals", key);
    }

  if (!def)
    {
      SCM vis = get_property ("explicitKeySignatureVisibility");
      if (ly_is_procedure (vis))
	item_->set_property ("break-visibility", vis);
    }
}

bool
Key_engraver::try_music (Music *ev)
{
  if (ev->is_mus_type ("key-change-event"))
    {
      /* do this only once, just to be on the safe side.  */
      if (!key_ev_)
	{
	  key_ev_ = ev;
	  read_ev (key_ev_);
	}
      return true;
    }
  return false;
}

void
Key_engraver::acknowledge_clef (Grob_info info)
{
  SCM c = get_property ("createKeyOnClefChange");
  if (to_boolean (c))
    {
      create_key (false);
    }
}

void
Key_engraver::acknowledge_bar_line (Grob_info info)
{
  if (scm_is_pair (get_property ("keySignature")))
    {
      create_key (true);
    }
}

void
Key_engraver::process_music ()
{
  if (key_ev_
      || get_property ("lastKeySignature") != get_property ("keySignature"))
    create_key (false);
}

void
Key_engraver::stop_translation_timestep ()
{
  item_ = 0;
  context ()->set_property ("lastKeySignature", get_property ("keySignature"));
  cancellation_ = 0;
  key_ev_ = 0;
}

void
Key_engraver::read_ev (Music const *r)
{
  SCM p = r->get_property ("pitch-alist");
  if (!scm_is_pair (p))
    return;

  SCM n = scm_list_copy (p);
  SCM accs = SCM_EOL;
  for (SCM s = get_property ("keyAccidentalOrder");
       scm_is_pair (s); s = scm_cdr (s))
    {
      if (scm_is_pair (scm_member (scm_car (s), n)))
	{
	  accs = scm_cons (scm_car (s), accs);
	  n = scm_delete_x (scm_car (s), n);
	}
    }

  for (SCM s = n; scm_is_pair (s); s = scm_cdr (s))
    if (scm_to_int (scm_cdar (s)))
      accs = scm_cons (scm_car (s), accs);

  context ()->set_property ("keySignature", accs);
  context ()->set_property ("tonic",
			    r->get_property ("tonic"));
}

void
Key_engraver::initialize ()
{
  context ()->set_property ("keySignature", SCM_EOL);
  context ()->set_property ("lastKeySignature", SCM_EOL);

  Pitch p (0, 0, 0);
  context ()->set_property ("tonic", p.smobbed_copy ());
}

#include "translator.icc"

ADD_ACKNOWLEDGER(Key_engraver,clef);
ADD_ACKNOWLEDGER(Key_engraver,bar_line);
ADD_TRANSLATOR (Key_engraver,
		/* descr */ "",
		/* creats*/ "KeySignature",
		/* accepts */ "key-change-event",
		/* acks  */ "",
		/* reads */ "keySignature printKeyCancellation lastKeySignature explicitKeySignatureVisibility createKeyOnClefChange keyAccidentalOrder keySignature",
		/* write */ "lastKeySignature tonic keySignature");
