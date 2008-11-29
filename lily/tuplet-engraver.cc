/*
  tuplet-engraver.cc -- implement Tuplet_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "beam.hh"
#include "engraver.hh"
#include "international.hh"
#include "note-column.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "tuplet-bracket.hh"
#include "warn.hh"
#include "item.hh"
#include "moment.hh"

#include "translator.icc"

struct Tuplet_description
{
  Stream_event *event_;
  Spanner *bracket_;
  Spanner *number_;

  bool full_length_;
  bool full_length_note_;
  Moment stop_moment_;
  Moment start_moment_;
  Moment length_;
  
  Tuplet_description ()
  {
    event_ = 0;
    full_length_note_ = false;
    full_length_ = false;
    bracket_ = 0;
    number_ = 0;
  }
};

class Tuplet_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Tuplet_engraver);

protected:
  vector<Tuplet_description> tuplets_;
  vector<Tuplet_description> new_tuplets_;
  vector<Tuplet_description> stopped_tuplets_;
  vector<Spanner*> last_tuplets_;
  
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_TRANSLATOR_LISTENER (tuplet_span);
  virtual void finalize ();
  void start_translation_timestep ();
  void process_music ();
};

IMPLEMENT_TRANSLATOR_LISTENER (Tuplet_engraver, tuplet_span);
void
Tuplet_engraver::listen_tuplet_span (Stream_event *ev)
{
  Direction dir = to_dir (ev->get_property ("span-direction"));
  if (dir == START)
    {
      Tuplet_description d;
      d.event_ = ev;

      d.length_ = robust_scm2moment (d.event_->get_property ("length"),
				     Moment (0));
      d.start_moment_ = now_mom ();
      d.stop_moment_ = now_mom () + d.length_;

      for (vsize i=0; i < new_tuplets_.size (); i++)
	{
	  /*
	    discard duplicates.
	  */
	  if (new_tuplets_[i].stop_moment_ == d.stop_moment_)
	    return;
	}
      
      new_tuplets_.push_back (d);
    }
  else if (dir == STOP)
    {
      if (tuplets_.size ())
	{
	  stopped_tuplets_.push_back (tuplets_.back ());
	  tuplets_.pop_back ();
	}
      else if (!to_boolean (get_property ("skipTypesetting"))) 
	ev->origin ()->warning (_ ("No tuplet to end"));
    }
  else 
    ev->origin ()->programming_error ("direction tuplet-span-event_ invalid.");
}

void
Tuplet_engraver::process_music ()
{
  /*
    This may happen if the end of a tuplet is part of a quoted voice.
   */
  Moment now = now_mom ();
  for (vsize i = tuplets_.size (); i --; )
    {
      if (tuplets_[i].stop_moment_ == now)
	{
	  stopped_tuplets_.push_back (tuplets_[i]);
	  tuplets_.erase (tuplets_.begin () + i);
	}
    }
  
  for (vsize i = 0; i < stopped_tuplets_.size (); i++)
    {
      Spanner *bracket = stopped_tuplets_[i].bracket_;
      Spanner *number = stopped_tuplets_[i].number_;
      if (bracket)
	{
	  if (stopped_tuplets_[i].full_length_)
	    {
	      Item *col =
		unsmob_item (stopped_tuplets_[i].full_length_note_
			     ? get_property ("currentMusicalColumn")
			     : get_property ("currentCommandColumn"));
	      
	      bracket->set_bound (RIGHT, col);
	      number->set_bound (RIGHT, col);
	    }
	  else if (!bracket->get_bound (RIGHT))
	    {
	      if (bracket->get_bound (LEFT))
		{
		  bracket->set_bound (RIGHT,
				      bracket->get_bound (LEFT));
		  number->set_bound (RIGHT,
				     stopped_tuplets_[i].bracket_->get_bound (LEFT));
		}
	      else
		programming_error ("stopped tuplet bracket has left nor right bound.");
	    }	  
	  // todo: scrap last_tuplets_, use stopped_tuplets_ only.
	  // clear stopped_tuplets_ at start_translation_timestep
	  last_tuplets_.push_back (bracket);
	  last_tuplets_.push_back (number);
	}
    }
  stopped_tuplets_.clear ();

  concat (tuplets_, new_tuplets_);
  new_tuplets_.clear ();
  for (vsize j = tuplets_.size (); j > 0; j--)
    {
      /* i goes from size-1 downto 0, inclusively */
      vsize i = j - 1;

            
      if (tuplets_[i].bracket_)
	continue;

      tuplets_[i].full_length_ = to_boolean (get_property ("tupletFullLength"));
      tuplets_[i].full_length_note_
	= to_boolean (get_property ("tupletFullLengthNote"));
      
      tuplets_[i].bracket_ = make_spanner ("TupletBracket",
					   tuplets_[i].event_->self_scm ());
      tuplets_[i].number_ = make_spanner ("TupletNumber",
					  tuplets_[i].event_->self_scm ());
      tuplets_[i].number_->set_object ("bracket", tuplets_[i].bracket_->self_scm ());
      tuplets_[i].bracket_->set_object ("tuplet-number", tuplets_[i].number_->self_scm ());
      tuplets_[i].stop_moment_.grace_part_ = 0;
      
      
      if (i + 1 < tuplets_.size () && tuplets_[i + 1].bracket_)
	Tuplet_bracket::add_tuplet_bracket (tuplets_[i].bracket_, tuplets_[i + 1].bracket_);
      
      if (i > 0 && tuplets_[i - 1].bracket_)
	Tuplet_bracket::add_tuplet_bracket (tuplets_[i - 1].bracket_, tuplets_[i].bracket_);

    }
}

void
Tuplet_engraver::acknowledge_note_column (Grob_info inf)
{
  for (vsize j = 0; j < tuplets_.size (); j++)
    if (tuplets_[j].bracket_)
      {
	Item *i = dynamic_cast<Item *> (inf.grob ());
	Tuplet_bracket::add_column (tuplets_[j].bracket_, i);
	add_bound_item (tuplets_[j].number_, i);
      }
}

void
Tuplet_engraver::start_translation_timestep ()
{
  last_tuplets_.clear ();
  /*
    May seem superfluous, but necessary for skipTypesetting.
   */
  new_tuplets_.clear ();	
}

void
Tuplet_engraver::finalize ()
{
  if (to_boolean (get_property ("tupletFullLength")))
    for (vsize i = 0; i < last_tuplets_.size (); i++)
      {
	Item *col = unsmob_item (get_property ("currentCommandColumn"));
	last_tuplets_[i]->set_bound (RIGHT, col);
      }
}

Tuplet_engraver::Tuplet_engraver ()
{
}

ADD_ACKNOWLEDGER (Tuplet_engraver, note_column);
ADD_TRANSLATOR (Tuplet_engraver,
		/* doc */
		"Catch tuplet events and generate appropriate bracket.",
		
		/* create */
		"TupletBracket "
		"TupletNumber ",

		/* read */
		"tupletFullLength "
		"tupletFullLengthNote ",

		/* write */
		""
		);
