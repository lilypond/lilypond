/*   
  staff-switching-translator.cc -- implement Staff_switching_translator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "engraver.hh"
#include "interpretation-context-handle.hh"
#include "drul-array.hh"
#include "engraver-group-engraver.hh"
#include "musical-request.hh"

class Staff_switching_translator : public Engraver
{
  Interpretation_context_handle my_voice_;
  Drul_array<Interpretation_context_handle> staff_handle_drul_;

  int switch_pitch_i_ ;
protected:
  virtual bool do_try_music (Music* m);
  virtual void do_creation_processing ();

  void default_voice (Direction);
public:
  Staff_switching_translator ();
  VIRTUAL_COPY_CONS(Translator);
};

Staff_switching_translator::Staff_switching_translator ()
{
  switch_pitch_i_ =0;
}

void
Staff_switching_translator::do_creation_processing ()
{
  Translator_group * daddy =daddy_grav_l (); // staff switching context


  Scalar pit = get_property ("switchPitch", 0);
  if (pit.isnum_b ())
    switch_pitch_i_ = int (pit);
  Scalar s = get_property("staffContextName", 0);
  Scalar up = get_property ("upStaffName",0);
  Scalar down = get_property ("downStaffName",0);
  if (!up.length_i()) up = "upper";
  if (!down.length_i()) up = "lower";  
  
  staff_handle_drul_[UP].set_translator (daddy->daddy_trans_l_ -> find_create_translator_l (s, up));
  staff_handle_drul_[DOWN].set_translator (daddy->daddy_trans_l_-> find_create_translator_l (s, down));  


  staff_handle_drul_[DOWN].report_to_l ()->set_property ("defaultClef", "bass");

  default_voice (UP);
}
  

void
Staff_switching_translator::default_voice (Direction d)
{
  Scalar s = get_property ("acceptorName",0);
  my_voice_.set_translator (staff_handle_drul_[d].report_to_l ()->find_create_translator_l ("Thread", daddy_trans_l_->id_str_));
}

bool
Staff_switching_translator::do_try_music (Music*m)
{
  if (Note_req*nr = dynamic_cast<Note_req*> (m))
    {
      Direction staff =  (nr->pitch_.semitone_pitch () >= switch_pitch_i_)
	? UP
	: DOWN;


      Translator_group * mv = my_voice_.report_to_l ();
      Translator_group *dest_staff =staff_handle_drul_[staff].report_to_l ();
      Scalar s = get_property ("switcherName", 0);
      
      while (mv && mv->type_str_ != s)
	{
	  mv = mv -> daddy_trans_l_;
	}

      if (!mv)
	default_voice (staff);

      if (mv->daddy_trans_l_  != dest_staff)
	{
	  mv->daddy_trans_l_->remove_translator_p (mv);
	  dest_staff->add_translator (mv);
	}
    }
  
  return my_voice_.try_music (m);
}

ADD_THIS_TRANSLATOR(Staff_switching_translator);
