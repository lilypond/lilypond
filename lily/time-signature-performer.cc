/*
  time-signature-performer.cc -- implement Time_signature_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "audio-item.hh"
#include "lily-proto.hh"
#include "performer.hh"


class Time_signature_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS (Translator);
  
  Time_signature_performer ();
  ~Time_signature_performer ();

protected:

  virtual void stop_translation_timestep ();
  virtual void create_audio_elements ();

  SCM prev_fraction_;
private:

  Audio_time_signature* audio_p_;
};


Time_signature_performer::Time_signature_performer ()
{
  prev_fraction_ = SCM_BOOL_F;
  audio_p_ = 0;
}

Time_signature_performer::~Time_signature_performer ()
{
}


void
Time_signature_performer::create_audio_elements ()
{
  SCM fr = get_property ("timeSignatureFraction");
  if (gh_pair_p (fr) && !gh_equal_p (fr, prev_fraction_))
    {
      prev_fraction_ = fr;
      int b = gh_scm2int (ly_car (fr));
      int o = gh_scm2int (ly_cdr (fr));
      
      audio_p_ = new Audio_time_signature (b,o);
      Audio_element_info info (audio_p_, 0);
      announce_element (info);

    }
}

void
Time_signature_performer::stop_translation_timestep ()
{
  if (audio_p_)
    {
      play_element (audio_p_);
      audio_p_ = 0;
    }
}
ADD_THIS_TRANSLATOR (Time_signature_performer);

