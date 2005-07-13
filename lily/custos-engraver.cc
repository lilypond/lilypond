/*
  custos-engraver.cc -- implement Custos_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2005 Juergen Reuter <reuter@ipd.uka.de>,
  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "engraver.hh"
#include "bar-line.hh"
#include "item.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"
#include "pitch.hh"


/*
 * This class implements an engraver for custos symbols.
 *
 * FIXME: note heads inside of ligatures (i.e. ligature heads) are
 * sometimes not recognized by this engraver. --jr
 */
class Custos_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Custos_engraver);
  virtual void start_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();
  virtual void finalize ();

private:
  Item *create_custos ();
  bool custos_permitted;
  Link_array<Grob> custodes_;
  Array<Pitch> pitches_;
};

Custos_engraver::Custos_engraver ()
{
  custos_permitted = false;
}

void
Custos_engraver::stop_translation_timestep ()
{
  /*
    delay typeset until we're at the next moment, so we can silence custodes at the end of the piece.
  */
  pitches_.clear ();

  custos_permitted = false;
}

void
Custos_engraver::start_translation_timestep ()
{
  custodes_.clear ();
}

void
Custos_engraver::acknowledge_grob (Grob_info info)
{
  Item *item = dynamic_cast<Item *> (info.grob ());
  if (item)
    {
      Music *m = info.music_cause ();
      if (Bar_line::has_interface (info.grob ()))
	custos_permitted = true;
      else if (Note_head::has_interface (info.grob ())
	       && m
	       && m->is_mus_type ("note-event"))
	{

	  /*
	    ideally, we'd do custos->set_parent (Y_AXIS, notehead),
	    but since the note head lives on the other system, we can't

	    So we copy the position from the note head pitch.  We
	    don't look at the staff-position, since we can't be sure
	    whether Clef_engraver already applied a vertical shift.
	  */
	  pitches_.push (*unsmob_pitch (m->get_property ("pitch")));
	}
    }
}

void
Custos_engraver::process_acknowledged_grobs ()
{
  if (scm_is_string (get_property ("whichBar")))
    custos_permitted = true;

  if (custos_permitted)
    {
      for (int i = pitches_.size (); i--;)
	{
	  Item *c = create_custos ();

	  int p = pitches_[i].steps ();
	  SCM c0 = get_property ("middleCPosition");
	  if (scm_is_number (c0))
	    p += scm_to_int (c0);

	  c->set_property ("staff-position",
			   scm_int2num (p));
	}

      pitches_.clear ();
    }
}

Item *
Custos_engraver::create_custos ()
{
  Item *custos = make_item ("Custos", SCM_EOL);

  custodes_.push (custos);

  return custos;
}

void
Custos_engraver::finalize ()
{
  for (int i = custodes_.size (); i--;)
    {
      custodes_[i]->suicide ();
    }
  custodes_.clear ();
}

ADD_TRANSLATOR (Custos_engraver,
		/* descr */ "",
		/* creats*/ "Custos",
		/* accepts */ "",
		/* acks  */ "bar-line-interface note-head-interface",
		/* reads */ "",
		/* write */ "");
