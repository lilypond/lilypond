/*
  text-spanner-engraver.cc -- implement Text_spanner_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "dimensions.hh"
#include "musical-request.hh"
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
  VIRTUAL_COPY_CONS (Translator);
  Text_spanner_engraver ();
  
protected:
  virtual void do_removal_processing ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music *);
  void deprecated_process_music ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void create_grobs ();

private:
  Spanner *span_;
  Spanner *finished_;
  Span_req *current_req_;
  Drul_array<Span_req*> req_drul_;
  void typeset_all ();
};

ADD_THIS_TRANSLATOR (Text_spanner_engraver);


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
  if (Span_req *s =  dynamic_cast <Span_req*> (m))
    {
      String t =  ly_scm2string (s->get_mus_property ("span-type"));            
      if (t == "abort")
	{
	  req_drul_[LEFT] = 0;
	  req_drul_[RIGHT] = 0;
	  if (span_)
	    span_->suicide ();
	  span_ = 0;
	}
      else if (t == "text")
	{
	  req_drul_[s->get_span_dir()] = s;
	  return true;
	}
    }
  return false;
}

void
Text_spanner_engraver::create_grobs ()
{
  deprecated_process_music ();
}

void
Text_spanner_engraver::deprecated_process_music ()
{
  /////
  if (req_drul_[STOP])
    {
      if (!span_)
	{
	  req_drul_[STOP]->origin ()->warning
	    (_ ("can't find start of text spanner"));
	}
      else
	{
	  assert (!finished_);
	  Grob* e = unsmob_element (get_property ("currentMusicalColumn"));
	  span_->set_bound (RIGHT, e);

	  finished_ = span_;
	  span_ = 0;
	  current_req_ = 0;
	}
    }

  if (req_drul_[START])
    {
      if (current_req_)
	{
	  req_drul_[START]->origin ()->warning
	    (_ ("already have a text spanner"));
	}
      else
	{
	  current_req_ = req_drul_[START];
	  span_  = new Spanner (get_property ("TextSpanner"));
	  Side_position::set_axis (span_, Y_AXIS);
	  Grob *e = unsmob_element (get_property ("currentMusicalColumn"));
	  span_->set_bound (LEFT, e);
	  announce_grob (span_, req_drul_[START]);
	}
    }
}

void
Text_spanner_engraver::acknowledge_grob (Grob_info info)
{
  if (span_ && Note_column::has_interface (info.elem_l_))
    {
      Side_position::add_support (span_, info.elem_l_);
      add_bound_item (span_, dynamic_cast<Item*> (info.elem_l_));
    }
}

void
Text_spanner_engraver::typeset_all ()
{  
  if (finished_)
    {
      Side_position::add_staff_support (finished_);
      typeset_grob (finished_);
      finished_ = 0;
    }
}

void
Text_spanner_engraver::stop_translation_timestep ()
{
  typeset_all ();
}

void
Text_spanner_engraver::do_removal_processing ()
{
  typeset_all ();
  if (span_)
    {
      current_req_->origin ()->warning (_ ("unterminated text spanner"));
      span_->suicide ();
      span_ = 0;
    }
}

