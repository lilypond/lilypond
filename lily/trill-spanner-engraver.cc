/*
  trill-spanner-engraver.cc -- implement Trill_spanner_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  C&P from text-spanner.cc

  - todo: ending should be detected automatically? a new note
    automatically is the end of the trill?
  
 */

#include "note-column.hh"
#include "side-position-interface.hh"
#include "engraver.hh"

class Trill_spanner_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Trill_spanner_engraver);  
protected:
  virtual void finalize ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music *);
  virtual void stop_translation_timestep ();
  virtual void process_music ();

private:
  Spanner *span_;
  Spanner *finished_;
  Music *current_req_;
  Drul_array<Music*> req_drul_;
  void typeset_all ();
};


Trill_spanner_engraver::Trill_spanner_engraver ()
{
  finished_ = 0;
  current_req_ = 0;
  span_ = 0;
  req_drul_[START] = 0;
  req_drul_[STOP] = 0;
}

bool
Trill_spanner_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("trill-span-event"))
    {
      Direction d = to_dir (m->get_property ("span-direction"));
      req_drul_[d] = m;
      return true;
    }

  return false;
}

void
Trill_spanner_engraver::process_music ()
{
  if (req_drul_[STOP])
    {
      if (!span_)
	{
	  req_drul_[STOP]->origin ()->warning (_ ("can't find start of trill spanner"));
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
	  req_drul_[START]->origin ()->warning (_ ("already have a trill spanner"));
	}
      else
	{
	  current_req_ = req_drul_[START];
	  span_  = make_spanner ("TrillSpanner", req_drul_[START]->self_scm ());
	  Side_position_interface::set_axis (span_, Y_AXIS);
	  req_drul_[START] = 0;
	}
    }
}

void
Trill_spanner_engraver::acknowledge_grob (Grob_info info)
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
Trill_spanner_engraver::typeset_all ()
{  
  if (finished_)
    {
      if (!finished_->get_bound (RIGHT))
	{
	  Grob* e = unsmob_grob (get_property ("currentMusicalColumn"));
	  finished_->set_bound (RIGHT, e);
	}
      finished_ = 0;
    }
}

void
Trill_spanner_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      Grob* e = unsmob_grob (get_property ("currentMusicalColumn"));
      span_->set_bound (LEFT, e);
    }

  typeset_all ();
  req_drul_[START] = 0;
  req_drul_[STOP] = 0;
}

void
Trill_spanner_engraver::finalize ()
{
  typeset_all ();
  if (span_)
    {
      current_req_->origin ()->warning (_ ("unterminated trill spanner"));
      span_->suicide ();
      span_ = 0;
    }
}

ADD_TRANSLATOR (Trill_spanner_engraver,
/* descr */       "Create trill spanner from a Music.",
/* creats*/       "TrillSpanner",
/* accepts */     "trill-span-event",
/* acks  */      "note-column-interface",
/* reads */       "",
/* write */       "");
