/*
  staff-performer.cc -- implement Staff_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "translator-group.hh"
#include "debug.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"
#include "performer-group-performer.hh"

/** Perform a staff. Individual notes should have their instrument
 (staff-wide) set, so we override play_element ()

  */
class Staff_performer : public Performer_group_performer 
{
public:
  TRANSLATOR_DECLARATIONS(Staff_performer);
  ~Staff_performer ();

  String new_instrument_str ();
  String instrument_str_;

protected:
  virtual void play_element (Audio_element* p);
  virtual void finalize ();
  virtual void initialize ();
  virtual void create_audio_elements ();
  virtual void stop_translation_timestep ();

private:
  Audio_staff* audio_staff_p_;
  Audio_instrument* instrument_p_;
  Audio_text* instrument_name_p_;
  Audio_text* name_p_;
  Audio_tempo* tempo_p_;
};

ENTER_DESCRIPTION (Staff_performer, "","","","","" );

Staff_performer::Staff_performer ()
{
  audio_staff_p_ = 0;
  instrument_p_ = 0;
  instrument_name_p_ = 0;
  name_p_ = 0;
  tempo_p_ = 0;
}

Staff_performer::~Staff_performer ()
{
}

void
Staff_performer::initialize ()
{
  audio_staff_p_ = new Audio_staff;
  announce_element (Audio_element_info (audio_staff_p_, 0));

  name_p_ = new Audio_text (Audio_text::TRACK_NAME, id_str_);
  announce_element (Audio_element_info (name_p_, 0));

  tempo_p_ = new Audio_tempo (get_tempo_i ());
  announce_element (Audio_element_info (tempo_p_, 0));

  Performer_group_performer::initialize ();
}

void
Staff_performer::create_audio_elements ()
{
  String str = new_instrument_str ();
  if (str.length_i ())
    {
      instrument_name_p_ = new Audio_text (Audio_text::INSTRUMENT_NAME, str);
      announce_element (Audio_element_info (instrument_name_p_, 0));
      instrument_p_ = new Audio_instrument (str);
      announce_element (Audio_element_info (instrument_p_, 0));
    }
  Performer_group_performer::create_audio_elements ();
}

void
Staff_performer::stop_translation_timestep ()
{
  SCM proc = scm_primitive_eval (ly_symbol2scm ("percussion-p")); 
  SCM drums_p = gh_call1 (proc, ly_symbol2scm (instrument_str_.ch_C()));
  audio_staff_p_->channel_i_ = (drums_p == SCM_BOOL_T ? 9 : -1 );
  if (name_p_)
    {
      play_element (name_p_);
      name_p_ = 0;
    }
  if (tempo_p_)
    {
      play_element (tempo_p_);
      tempo_p_ = 0;
    }
  if (instrument_name_p_)
    {
      play_element (instrument_name_p_);
      instrument_name_p_ = 0;
    }
  if (instrument_p_)
    {
      play_element (instrument_p_);
      instrument_p_ = 0;
    }
  Performer_group_performer::stop_translation_timestep ();
}

void
Staff_performer::finalize ()
{
  Performer_group_performer::finalize ();
  Performer::play_element (audio_staff_p_);
  audio_staff_p_ = 0;
}

String 
Staff_performer::new_instrument_str () 
{ 
  // mustn't ask Score for instrument: it will return piano!
  SCM minstr = get_property (ly_symbol2scm ("midiInstrument"));

  if (!gh_string_p (minstr))
    minstr = get_property (ly_symbol2scm ("instrument"));

  if (!gh_string_p (minstr)
      || ly_scm2string (minstr) == instrument_str_)
    return "";

  instrument_str_ = ly_scm2string (minstr);

  return instrument_str_;
}

void 
Staff_performer::play_element (Audio_element* p)
{
  if (Audio_item *ai = dynamic_cast<Audio_item *> (p)) 
    {
      audio_staff_p_->add_audio_item (ai);
    }
  Performer::play_element (p);
}

