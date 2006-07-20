/*
  tuplet-engraver.cc -- implement Tuplet_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "tuplet-bracket.hh"
#include "note-column.hh"
#include "beam.hh"
#include "engraver.hh"
#include "spanner.hh"

#include "translator.icc"

struct Tuplet_description
{
  Music *music_;
  Spanner *bracket_;
  Spanner *number_;

  bool full_length_;
  bool full_length_note_;
  
  Tuplet_description ()
  {
    full_length_note_ = false;
    full_length_ = false;
    music_ = 0;
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
  vector<Tuplet_description> stopped_tuplets_;
  vector<Spanner*> last_tuplets_;
  DECLARE_ACKNOWLEDGER (note_column);
  virtual bool try_music (Music *r);
  virtual void finalize ();
  void start_translation_timestep ();
  void process_music ();
};

bool
Tuplet_engraver::try_music (Music *music)
{
  if (music->is_mus_type ("tuplet-spanner-event"))
    {
      Direction dir = to_dir (music->get_property ("span-direction"));
      if (dir == START)
	{
	  Tuplet_description d;
	  d.music_ = music;
	  tuplets_.push_back (d);
	}
      if (dir == STOP)
	{
	  stopped_tuplets_.push_back (tuplets_.back ());
	  tuplets_.pop_back ();
	}
      return true;
    }
  return false;
}

void
Tuplet_engraver::process_music ()
{
  for (vsize i = 0; i < stopped_tuplets_.size (); i++)
    {
      if (stopped_tuplets_[i].bracket_)
	{
	  if (stopped_tuplets_[i].full_length_)
	    {
	      Item *col =
		unsmob_item (stopped_tuplets_[i].full_length_note_
			     ? get_property ("currentMusicalColumn")
			     : get_property ("currentCommandColumn"));
	      
	      stopped_tuplets_[i].bracket_->set_bound (RIGHT, col);
	      stopped_tuplets_[i].number_->set_bound (RIGHT, col);
	    }
	  else if (!stopped_tuplets_[i].bracket_->get_bound (RIGHT))
	    {
	      stopped_tuplets_[i].bracket_->set_bound (RIGHT,
						       stopped_tuplets_[i].bracket_->get_bound (LEFT));
	      stopped_tuplets_[i].number_->set_bound (RIGHT,
						      stopped_tuplets_[i].bracket_->get_bound (LEFT));
	    }
	  // todo: scrap last_tuplets_, use stopped_tuplets_ only.
	  // clear stopped_tuplets_ at start_translation_timestep
	  last_tuplets_.push_back (tuplets_[i].bracket_);
	  last_tuplets_.push_back (tuplets_[i].number_);
	}
    }
  stopped_tuplets_.clear ();

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
					   tuplets_[i].music_->self_scm ());
      tuplets_[i].number_ = make_spanner ("TupletNumber",
					  tuplets_[i].music_->self_scm ());
      tuplets_[i].number_->set_object ("bracket", tuplets_[i].bracket_->self_scm ());
      tuplets_[i].bracket_->set_object ("tuplet-number", tuplets_[i].number_->self_scm ());
      
      if (i < tuplets_.size () - 1 && tuplets_[i + 1].bracket_)
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
		/* doc */ "Catch TupletSpannerEvent and generate appropriate bracket  ",
		/* create */ "TupletBracket TupletNumber ",
		/* accept */ "tuplet-spanner-event",
		/* read */ "tupletFullLength ",
		/* write */ "");
