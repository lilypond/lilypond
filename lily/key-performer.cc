/*
  key-performer.cc -- implement Key_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lily-guile.hh"

#include "audio-item.hh"
#include "performer.hh"
#include "warn.hh"


class Key_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Key_performer);
  ~Key_performer ();

protected:
  virtual bool try_music (Music* req);
  virtual void create_audio_elements ();
  virtual void stop_translation_timestep ();

private:
  Key_change_ev* key_req_;
  Audio_key* audio_;
};

Key_performer::Key_performer ()
{
  key_req_ = 0;
  audio_ = 0;
}

Key_performer::~Key_performer ()
{
}

void
Key_performer::create_audio_elements ()
{
  if (key_req_) 
    {
      SCM pitchlist = key_req_->get_property ("pitch-alist");
      SCM proc = ly_scheme_function ("alterations-in-key");
      
      SCM acc = scm_call_1 (proc, pitchlist);
      
      Pitch key_do (0, 
		    scm_to_int (scm_caar (pitchlist)),
		    scm_to_int (scm_cdar (pitchlist)));

      Pitch c_do (0, 0, 0);
		  
      SCM c_pitchlist
	= ly_transpose_key_alist (pitchlist,
				  pitch_interval (key_do, c_do).smobbed_copy ());

      /* MIDI keys are too limited for lilypond scales.
	 We check for minor scale and assume major otherwise.  */
      SCM minor = scm_c_eval_string ("minor");
      audio_ = new Audio_key (scm_to_int (acc),
			      SCM_BOOL_T != scm_equal_p (minor, c_pitchlist));

      Audio_element_info info (audio_, key_req_);
      announce_element (info);
      key_req_ = 0;
    }
}

void
Key_performer::stop_translation_timestep ()
{
  if (audio_)
    {
      play_element (audio_);
      audio_ = 0;
    }
}

bool
Key_performer::try_music (Music* req)
{
  if (Key_change_ev *kc = dynamic_cast <Key_change_ev *> (req))
    {
      if (key_req_)
	warning (_ ("FIXME: key change merge"));

      key_req_ = kc;
      return true;
    }

  return false;
}

ENTER_DESCRIPTION (Key_performer,
		  "","",
		  "key-change-event",
		  "","","");
