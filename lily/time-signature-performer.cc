/*
  time-signature-performer.cc -- implement Time_signature_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "audio-item.hh"
#include "performer.hh"
#include "protected-scm.hh"

class Time_signature_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Time_signature_performer);
  ~Time_signature_performer ();

protected:

  void stop_translation_timestep ();
  void process_music ();
  virtual void derived_mark () const;
  SCM prev_fraction_;
private:

  Audio_time_signature *audio_;
};

void
Time_signature_performer::derived_mark () const
{
  scm_gc_mark (prev_fraction_);
}

Time_signature_performer::Time_signature_performer ()
{
  prev_fraction_ = SCM_BOOL_F;
  audio_ = 0;
}

Time_signature_performer::~Time_signature_performer ()
{
}

void
Time_signature_performer::process_music ()
{
  SCM fr = get_property ("timeSignatureFraction");
  if (scm_is_pair (fr) && !ly_is_equal (fr, prev_fraction_))
    {
      prev_fraction_ = fr;
      int b = scm_to_int (scm_car (fr));
      int o = scm_to_int (scm_cdr (fr));

      audio_ = new Audio_time_signature (b, o);
      Audio_element_info info (audio_, 0);
      announce_element (info);
    }
}

void
Time_signature_performer::stop_translation_timestep ()
{
  if (audio_)
    {
      audio_ = 0;
    }
}

#include "translator.icc"

ADD_TRANSLATOR (Time_signature_performer,
		/* doc */
		"",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
