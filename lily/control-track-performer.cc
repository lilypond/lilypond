

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

  void add_text (Audio_text::Type, const string&);
  TRANSLATOR_DECLARATIONS (Control_track_performer);
protected:

  virtual void initialize ();
  virtual void acknowledge_audio_element (Audio_element_info info);
  virtual void finalize ();
};

Control_track_performer::Control_track_performer (Context *c)
  : Performer (c)
{
  control_track_ = 0;
}

void
Control_track_performer::acknowledge_audio_element (Audio_element_info info)
{
  if (Audio_tempo *tempo = dynamic_cast<Audio_tempo *> (info.elem_))
    {
      control_track_->add_audio_item (tempo);
    }
  if (Audio_time_signature *sig = dynamic_cast<Audio_time_signature *> (info.elem_))
    {
      control_track_->add_audio_item (sig);
    }
}

void
Control_track_performer::add_text (Audio_text::Type text_type, const string &str)
{
  Audio_item *text = new Audio_text (text_type, str);
  control_track_->add_audio_item (text);

  announce_element (Audio_element_info (text, 0));

}

void
Control_track_performer::initialize ()
{
  control_track_ = new Audio_control_track_staff;
  announce_element (Audio_element_info (control_track_, 0));

  string id_string = String_convert::pad_to (gnu_lilypond_version_string (), 30);

  // The first audio element in the control track is a placeholder for the
  // name of the MIDI sequence.  The actual name is stored in the element
  // later before outputting the track (in Performance::output, see
  // performance.cc).
  add_text (Audio_text::TRACK_NAME, "control track");
  add_text (Audio_text::TEXT, "creator: ");
  add_text (Audio_text::TEXT, id_string);
}

void
Control_track_performer::finalize ()
{
  control_track_->end_mom_ = now_mom ();
}

void
Control_track_performer::boot ()
{

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
