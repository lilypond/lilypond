

#include "warn.hh"
#include "audio-item.hh"
#include "audio-staff.hh"
#include "performer.hh"
#include "string-convert.hh"
#include "lily-version.hh"

#include "translator.icc"

class Control_track_performer : public Performer
{
  Audio_staff *control_track_;
  vector<Audio_item*> texts_;

  void add_text (Audio_text::Type, string);
  TRANSLATOR_DECLARATIONS (Control_track_performer);
protected:

  virtual void initialize ();
  virtual void acknowledge_audio_element (Audio_element_info info);
};


Control_track_performer::Control_track_performer ()
{
  control_track_ = 0;
}

void
Control_track_performer::acknowledge_audio_element (Audio_element_info info)
{
  if (Audio_tempo *tempo = dynamic_cast<Audio_tempo*> (info.elem_))
    {
      control_track_->add_audio_item (tempo);
    }
  if (Audio_time_signature * sig = dynamic_cast<Audio_time_signature *> (info.elem_))
    {
      control_track_->add_audio_item (sig);
    }
}

void
Control_track_performer::add_text (Audio_text::Type text_type, string str)
{
  Audio_item *text = new Audio_text (text_type, str);
  control_track_->add_audio_item (text);
  texts_.push_back (text);

  announce_element (Audio_element_info (text, 0));
  
}

void
Control_track_performer::initialize ()
{
  control_track_ = new Audio_staff;
  announce_element (Audio_element_info (control_track_, 0));

  string id_string = String_convert::pad_to (gnu_lilypond_version_string (), 30);
  
  add_text (Audio_text::TRACK_NAME, "control track");
  add_text (Audio_text::TEXT, "creator: ");
  add_text (Audio_text::TEXT, id_string);
}

ADD_TRANSLATOR (Control_track_performer,
		/* doc */
		"",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
