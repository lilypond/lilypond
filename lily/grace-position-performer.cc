/*   
  grace-position-performer.cc --  implement Grace_position_performer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>

 */

#include "performer.hh"
#include "audio-item.hh"
#include "global-translator.hh"

class Grace_position_performer : public Performer
{
public:
  Grace_position_performer ();

protected:
  Link_array<Audio_note> graces_;
  Link_array<Audio_note> notes_;

  VIRTUAL_COPY_CONS (Translator);
  virtual void acknowledge_audio_element (Audio_element_info);
  virtual void create_audio_elements ();
  virtual void start_translation_timestep ();
  Global_translator* global_translator_l ();
};

ADD_THIS_TRANSLATOR (Grace_position_performer);

Grace_position_performer::Grace_position_performer ()
{
}

void
Grace_position_performer::acknowledge_audio_element (Audio_element_info i)
{
  if (Audio_note * n = dynamic_cast <Audio_note*> (i.elem_l_))
    {
      if (i.elem_l_->grace_b_)
	graces_.push (n);
      else
	notes_.push (n);
    }
}

void
Grace_position_performer::create_audio_elements ()
{
  if (graces_.size ())
    {
      // we're above grace-engraver-group, so we cannot tell
      // grace-iterator.  note-performer should add moments.
      //Global_translator* global_l = global_translator_l ();
      Moment delay_mom = Moment (1, 8);
      if (notes_.size ())
	{
	  Moment shortest_mom = notes_[0]->length_mom_;
	  for (int i=1; i < notes_.size (); i++)
	    shortest_mom = shortest_mom <? notes_[i]->length_mom_;
	  
	  Rational grace_fraction_rat (1, 2);
	  SCM prop = get_property ("graceFraction");
	  if (unsmob_moment (prop))
	    grace_fraction_rat = *unsmob_moment (prop);

	  delay_mom = shortest_mom * grace_fraction_rat;
	  for (int i=0; i < notes_.size (); i++)
	    {
	      Audio_note* n = notes_[i];
	      n->length_mom_ -= delay_mom;
	      n->delayed_mom_ = delay_mom;
	      n->delayed_until_mom_ = now_mom () + delay_mom;
	      //global_l->add_moment_to_process (n->delayed_until_mom_);
	    }
	  notes_.clear ();
	}
      
      Moment grace_length_mom;
      for (int i=0; i < graces_.size (); i++)
	grace_length_mom += graces_[i]->length_mom_;

      Rational grace_factor_rat = delay_mom / grace_length_mom;

      for (int i=0; i < graces_.size (); i++)
	{
	  Audio_note* n = graces_[i];
	  n->length_mom_ *= grace_factor_rat;
	  if (i)
	    {
	      Audio_note* p = graces_[i-1];
	      n->delayed_mom_ = p->delayed_mom_ + p->length_mom_;
	      n->delayed_until_mom_ = now_mom () + n->delayed_mom_;
	      //global_l->add_moment_to_process (n->delayed_until_mom_);
	    }
	}
      graces_.clear ();
    }
}

Global_translator*
Grace_position_performer::global_translator_l ()
{
  Translator *t = this;
  Global_translator *global_l =0;
  do
    {
      t = t->daddy_trans_l_ ;
      global_l = dynamic_cast<Global_translator*> (t);
    }
  while (!global_l);

  return global_l;
}


void
Grace_position_performer::start_translation_timestep ()
{
  graces_.clear ();
  notes_.clear ();
}

