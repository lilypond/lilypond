/*
  hyphen-engraver.cc -- implement Hyphen_engraver

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>
*/

#include "proto.hh"
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
  Score_element *last_lyric_l_;
  Score_element *current_lyric_l_;
  Hyphen_req* req_l_;
  Spanner* hyphen_p_;
public:
  Hyphen_engraver ();
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_removal_processing();
  virtual void do_process_music();
  virtual bool do_try_music (Music*);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing ();
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
Hyphen_engraver::acknowledge_element (Score_element_info i)
{
  // -> text-item
  if (i.elem_l_->has_interface (ly_symbol2scm ("text-item-interface")))
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
Hyphen_engraver::do_try_music (Music* r)
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
Hyphen_engraver::do_removal_processing ()
{
  if (hyphen_p_)
    {
      req_l_->warning (_ ("unterminated hyphen"));
      hyphen_p_->set_bound(RIGHT, unsmob_element (get_property ("currentCommandColumn")));
    }
}

void
Hyphen_engraver::do_process_music ()
{
  if (req_l_)
    {
      if (!last_lyric_l_)
	{
	  req_l_->warning (_ ("Nothing to connect hyphen to on the left.  Ignoring hyphen request."));
	  return;
	}
      
      hyphen_p_ = new Spanner (get_property ("basicHyphenSpannerProperties"));
      hyphen_p_->set_extent_callback (Score_element::point_dimension_callback,Y_AXIS);
      Hyphen_spanner (hyphen_p_).set_textitem  (LEFT, last_lyric_l_);
      announce_element (Score_element_info (hyphen_p_, req_l_));
    }
}


void
Hyphen_engraver::do_pre_move_processing ()
{
  if (hyphen_p_)
    {
      typeset_element (hyphen_p_);
      hyphen_p_ = 0;
    }

  if (current_lyric_l_)
    {
      last_lyric_l_ = current_lyric_l_;
      current_lyric_l_ =0;
    }
}

void
Hyphen_engraver::do_post_move_processing ()
{
  req_l_ = 0;
}


