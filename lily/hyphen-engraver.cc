/*
  hyphen-engraver.cc -- implement Hyphen_engraver

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>
*/

#include "flower-proto.hh"
#include "musical-request.hh"
#include "hyphen-spanner.hh"
#include "paper-column.hh"
#include "item.hh"
#include "engraver.hh"

/**
  Generate an centred hyphen.  Should make a Hyphen_spanner that
  typesets a nice centred hyphen of varying length depending on the
  gap between syllables.

  We remember the last Item that come across. When we get a
  request, we create the spanner, and attach the left point to the
  last lyrics, and the right point to any lyrics we receive by
  then.  */
class Hyphen_engraver : public Engraver
{
  Grob *last_lyric_l_;
  Grob *current_lyric_l_;
  Hyphen_req* req_l_;
  Spanner* hyphen_p_;
public:
  Hyphen_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void finalize();
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep();
  virtual void start_translation_timestep ();
  virtual void create_grobs ();
private:

};

ADD_THIS_TRANSLATOR (Hyphen_engraver);

Hyphen_engraver::Hyphen_engraver ()
{
  current_lyric_l_ = 0;
  last_lyric_l_ = 0;
  hyphen_p_ = 0;
  req_l_ = 0;
}

void
Hyphen_engraver::acknowledge_grob (Grob_info i)
{
  // -> text-item
  if (i.elem_l_->has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    {
      current_lyric_l_ = i.elem_l_;
      if (hyphen_p_
	  && !hyphen_p_->get_bound (RIGHT)
	    )
	  {
	    Hyphen_spanner (hyphen_p_).set_textitem (RIGHT, i.elem_l_);
	  }
    }
}


bool
Hyphen_engraver::try_music (Music* r)
{
  if (Hyphen_req* p = dynamic_cast <Hyphen_req *> (r))
    {
      if (req_l_)
	return false;

      req_l_ = p;
      return true;
    }
  return false;
}

void
Hyphen_engraver::finalize ()
{
  if (hyphen_p_)
    {
      req_l_->origin ()->warning (_ ("unterminated hyphen"));
      hyphen_p_->set_bound(RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
    }
}

void
Hyphen_engraver::create_grobs ()
{
  if (req_l_ &&! hyphen_p_)
    {
      if (!last_lyric_l_)
	{
	  req_l_->origin ()->warning (_ ("Nothing to connect hyphen to on the left.  Ignoring hyphen request."));
	  return;
	}
      
      hyphen_p_ = new Spanner (get_property ("LyricHyphen"));

      Hyphen_spanner (hyphen_p_).set_textitem  (LEFT, last_lyric_l_);
      announce_grob (hyphen_p_, req_l_);
    }
}


void
Hyphen_engraver::stop_translation_timestep ()
{
  if (hyphen_p_)
    {
      typeset_grob (hyphen_p_);
      hyphen_p_ = 0;
    }

  if (current_lyric_l_)
    {
      last_lyric_l_ = current_lyric_l_;
      current_lyric_l_ =0;
    }
}

void
Hyphen_engraver::start_translation_timestep ()
{
  req_l_ = 0;
}


