/*
  text-spanner-engraver.cc -- implement Text_spanner_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "dimensions.hh"
#include "request.hh"
#include "paper-column.hh"
#include "note-column.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "translator-group.hh"
#include "axis-group-interface.hh"


class Text_spanner_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS(Text_spanner_engraver);  
protected:
  virtual void finalize ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music *);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();

private:
  Spanner *span_;
  Spanner *finished_;
  Music *current_req_;
  Drul_array<Music*> req_drul_;
  void typeset_all ();
};


Text_spanner_engraver::Text_spanner_engraver ()
{
  finished_ = 0;
  current_req_ = 0;
  span_ =0;
  req_drul_[START] = 0;
  req_drul_[STOP] = 0;
}

void
Text_spanner_engraver::start_translation_timestep ()
{
  req_drul_[START] = 0;
  req_drul_[STOP] = 0;
}

bool
Text_spanner_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("text-span-event"))
    {

      Direction d = to_dir (m->get_mus_property ("span-direction"));
      req_drul_[d] = m;
      return true;
    }
  else if (m->is_mus_type ("abort-event"))
    {
      req_drul_[LEFT] = 0;
      req_drul_[RIGHT] = 0;
      if (span_)
	span_->suicide ();
      span_ = 0;
    }

  return false;
}

void
Text_spanner_engraver::process_music ()
{
  if (req_drul_[STOP])
    {
      if (!span_)
	{
	  req_drul_[STOP]->origin ()->warning
 (_ ("can't find start of text spanner"));
	}
      else
	{
	  finished_ = span_;
	  span_ = 0;
	  current_req_ = 0;
	}
    }

  if (req_drul_[START])
    {
      if (current_req_)
	{
	  req_drul_[START]->origin ()->warning(_ ("already have a text spanner"));
	}
      else
	{
	  current_req_ = req_drul_[START];
	  span_  = new Spanner (get_property ("TextSpanner"));

	  /* Ugh.  Reset (de)cresc. specific properties */
	  span_->set_grob_property ("outer", SCM_BOOL_T);
	  span_->set_grob_property ("if-text-padding", gh_double2scm (0));
	  span_->set_grob_property ("width-correct", gh_double2scm (0));
	  	    
	  Side_position_interface::set_axis (span_, Y_AXIS);
	  announce_grob (span_, req_drul_[START]->self_scm());
	  req_drul_[START] = 0;
	}
    }
}

void
Text_spanner_engraver::acknowledge_grob (Grob_info info)
{
  Spanner * spans[2] ={span_, finished_};
  for (int i = 0;  i < 2 ; i++)
    {
      if (spans[i] && Note_column::has_interface (info.grob_))
	{
	  Side_position_interface::add_support (spans[i], info.grob_);
	  add_bound_item (spans[i], dynamic_cast<Item*> (info.grob_));
	}
    }
}

void
Text_spanner_engraver::typeset_all ()
{  
  if (finished_)
    {
      Side_position_interface::add_staff_support (finished_);
      if (!finished_->get_bound (RIGHT))
	{
	  Grob* e = unsmob_grob (get_property ("currentMusicalColumn"));
	  finished_->set_bound (RIGHT, e);
	}
      typeset_grob (finished_);
      finished_ = 0;
    }
}

void
Text_spanner_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      Grob* e = unsmob_grob (get_property ("currentMusicalColumn"));
      span_->set_bound (LEFT, e);
    }

  typeset_all ();
}

void
Text_spanner_engraver::finalize ()
{
  typeset_all ();
  if (span_)
    {
      current_req_->origin ()->warning (_ ("unterminated text spanner"));
      span_->suicide ();
      span_ = 0;
    }
}

ENTER_DESCRIPTION(Text_spanner_engraver,
/* descr */       "Create text spanner from a Music.",
/* creats*/       "TextSpanner",
/* accepts */     "text-span-event",
/* acks  */      "note-column-interface",
/* reads */       "",
/* write */       "");
