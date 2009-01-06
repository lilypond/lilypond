/*
  piano-pedal-engraver.cc -- implement Piano_pedal_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>,
                 Erik Sandberg <mandolaerik@gmail.com>

  Chris Jackson <chris@fluffhouse.org.uk> - extended to support
  bracketed pedals.
*/

#include "engraver.hh"

#include "axis-group-interface.hh"
#include "context.hh"
#include "directional-element-interface.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "note-column.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "spanner.hh"
#include "item.hh"

#include "translator.icc"

#include <string.h>

/*
  TODO:

  * Junk hardcoded sustain/sostenuto/una_corda distinction;
    Softcode using (list (sustain-event SustainPedal PianoPedalBracket) ... )

  * Try to use same engraver for dynamics.
  
*/

/* Ugh: This declaration is duplicated in piano-pedal-performer */
enum Pedal_type {
  SOSTENUTO,
  SUSTAIN,
  UNA_CORDA,
  NUM_PEDAL_TYPES
};

/*
  Static precalculated data (symbols and strings) for the different
  pedal types
*/
struct Pedal_type_info
{
  string base_name_;
  SCM event_class_sym_;
  SCM style_sym_;
  SCM strings_sym_;
  
  const char *pedal_c_str_;

  Pedal_type_info ()
  {
    event_class_sym_ = SCM_EOL;
    style_sym_ = SCM_EOL;
    strings_sym_ = SCM_EOL;
    pedal_c_str_ = 0;
  }
  void protect ()
  {
    scm_gc_protect_object (event_class_sym_);
    scm_gc_protect_object (style_sym_);
    scm_gc_protect_object (strings_sym_);
  }
};

struct Pedal_info
{
  const Pedal_type_info *type_;

  /*
    Event for currently running pedal.
  */
  Stream_event *current_bracket_ev_;

  /*
    Event for currently starting pedal, (necessary?

    distinct from current_bracket_ev_, since current_bracket_ev_ only
    necessary for brackets, not for text style.
  */
  Stream_event *start_ev_;

  /*
    Events that were found in this timestep.
  */
  Drul_array<Stream_event *> event_drul_;
  Item *item_;
  Spanner *bracket_; // A single portion of a pedal bracket
  Spanner *finished_bracket_;
};

static Pedal_type_info pedal_types_[NUM_PEDAL_TYPES];

class Piano_pedal_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Piano_pedal_engraver);

protected:
  virtual void initialize ();
  virtual void finalize ();
  DECLARE_TRANSLATOR_LISTENER (sustain);
  DECLARE_TRANSLATOR_LISTENER (una_corda);
  DECLARE_TRANSLATOR_LISTENER (sostenuto);
  DECLARE_ACKNOWLEDGER (note_column);
  void stop_translation_timestep ();
  void process_music ();

private:
  Pedal_info info_list_[NUM_PEDAL_TYPES + 1];

  void create_text_grobs (Pedal_info *p, bool);
  void create_bracket_grobs (Pedal_info *p, bool);
  void typeset_all (Pedal_info *p);
};


static void
init_pedal_types ()
{
  const char *names [NUM_PEDAL_TYPES];
  names[SOSTENUTO] = "Sostenuto";
  names[SUSTAIN] = "Sustain";
  names[UNA_CORDA] = "UnaCorda";

  for (int i = 0; i < NUM_PEDAL_TYPES; i++)
    {
      const char *name = names[i];
      /* FooBar */
      string base_name = name;
      /* foo-bar */
      string base_ident = "";
      int prev_pos=0;
      int cur_pos;
      for (cur_pos = 1; name[cur_pos]; cur_pos++)
	if (isupper (name[cur_pos]))
	  {
	    base_ident = base_ident + String_convert::to_lower (string (name, prev_pos, cur_pos - prev_pos)) + "-";
	    prev_pos = cur_pos;
	  }
      base_ident += String_convert::to_lower (string (name, prev_pos, cur_pos - prev_pos));

      /*
	be careful, as we don't want to loose references to the _sym_ members.
       */
      Pedal_type_info info;
      info.event_class_sym_ = scm_str2symbol ((base_ident + "-event").c_str ());
      info.style_sym_ = scm_str2symbol (("pedal" + base_name + "Style").c_str ());
      info.strings_sym_ = scm_str2symbol (("pedal" + base_name + "Strings").c_str ());
      
      info.base_name_ = name;
      info.pedal_c_str_ = strdup ((base_name + "Pedal").c_str ());

      info.protect ();
      
      pedal_types_[i] = info;
    }
}

ADD_SCM_INIT_FUNC (Piano_pedal_engraver_init_pedal_types_, init_pedal_types);

Piano_pedal_engraver::Piano_pedal_engraver ()
{
}

void
Piano_pedal_engraver::initialize ()
{
  for (int i = 0; i < NUM_PEDAL_TYPES; i++)
    {
      Pedal_type_info *s = &pedal_types_[i];
      Pedal_info *info = &info_list_[i];

      info->type_ = s;
      info->item_ = 0;
      info->bracket_ = 0;
      info->finished_bracket_ = 0;
      info->current_bracket_ev_ = 0;
      info->event_drul_[START] = 0;
      info->event_drul_[STOP] = 0;
      info->start_ev_ = 0;
    }
  info_list_[NUM_PEDAL_TYPES].type_ = 0;
}


/*
  Urg: Code dup
  I'm a script
*/
void
Piano_pedal_engraver::acknowledge_note_column (Grob_info info)
{
  for (Pedal_info *p = info_list_; p->type_; p++)
    {
      if (p->bracket_)
	add_bound_item (p->bracket_, info.grob ());
      if (p->finished_bracket_)
	add_bound_item (p->finished_bracket_, info.grob ());
    }
}

IMPLEMENT_TRANSLATOR_LISTENER (Piano_pedal_engraver, sostenuto);
void
Piano_pedal_engraver::listen_sostenuto (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  ASSIGN_EVENT_ONCE (info_list_[SOSTENUTO].event_drul_[d], ev);
}

IMPLEMENT_TRANSLATOR_LISTENER (Piano_pedal_engraver, sustain);
void
Piano_pedal_engraver::listen_sustain (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  ASSIGN_EVENT_ONCE (info_list_[SUSTAIN].event_drul_[d], ev);
}

IMPLEMENT_TRANSLATOR_LISTENER (Piano_pedal_engraver, una_corda);
void
Piano_pedal_engraver::listen_una_corda (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  ASSIGN_EVENT_ONCE (info_list_[UNA_CORDA].event_drul_[d], ev);
}

void
Piano_pedal_engraver::process_music ()
{
  for (Pedal_info *p = info_list_; p->type_; p++)
    {
      if (p->event_drul_[STOP] || p->event_drul_[START])
	{
	  /* Choose the appropriate grobs to add to the line spanner
	     These can be text items or text-spanners
	  */

	  /*
	    ugh, code dup, should read grob to create from other
	    property.

	    bracket: |_________/\____|
	    text:    Ped.     *Ped.  *
	    mixed:   Ped. _____/\____|
	  */

	  SCM style = internal_get_property (p->type_->style_sym_);

	  bool mixed = style == ly_symbol2scm ("mixed");
	  bool bracket = (mixed
			  || style == ly_symbol2scm ("bracket"));
	  bool text = (style == ly_symbol2scm ("text")
		       || mixed);

	  if (text && !p->item_)
	    create_text_grobs (p, mixed);
	  if (bracket)
	    create_bracket_grobs (p, mixed);
	}
    }
}

void
Piano_pedal_engraver::create_text_grobs (Pedal_info *p, bool mixed)
{
  SCM s = SCM_EOL;
  SCM strings = internal_get_property (p->type_->strings_sym_);

  if (scm_ilength (strings) < 3)
    {
      Stream_event *m = p->event_drul_[START];
      if (!m) m = p->event_drul_ [STOP];

      string msg = _f ("expect 3 strings for piano pedals, found: %ld",
		       scm_ilength (strings));
      if (m)
	m->origin ()->warning (msg);
      else
	warning (msg);

      return;
    }

  if (p->event_drul_[STOP] && p->event_drul_[START])
    {
      if (!mixed)
	{
	  if (!p->start_ev_)
	    p->event_drul_[STOP]->origin ()->warning (_f ("cannot find start of piano pedal: `%s'", p->type_->base_name_.c_str ()));
	  else
	    s = scm_cadr (strings);
	  p->start_ev_ = p->event_drul_[START];
	}
    }
  else if (p->event_drul_[STOP])
    {
      if (!mixed)
	{
	  if (!p->start_ev_)
	    p->event_drul_[STOP]->origin ()->warning (_f ("cannot find start of piano pedal: `%s'", p->type_->base_name_.c_str ()));
	  else
	    s = scm_caddr (strings);
	  p->start_ev_ = 0;
	}
    }
  else if (p->event_drul_[START])
    {
      p->start_ev_ = p->event_drul_[START];
      s = scm_car (strings);
      }

  if (scm_is_string (s))
    {
      const char *propname = p->type_->pedal_c_str_;

      p->item_ = make_item (propname, (p->event_drul_[START]
				       ? p->event_drul_[START]
				       : p->event_drul_[STOP])->self_scm ());

      p->item_->set_property ("text", s);
    }

  if (!mixed)
    {
      p->event_drul_[START] = 0;
      p->event_drul_[STOP] = 0;
    }
}

void
Piano_pedal_engraver::create_bracket_grobs (Pedal_info *p, bool mixed)
{
  if (!p->bracket_ && p->event_drul_[STOP])
    {
      string msg = _f ("cannot find start of piano pedal bracket: `%s'", p->type_->base_name_.c_str ());
      p->event_drul_[STOP]->origin ()->warning (msg);
      p->event_drul_[STOP] = 0;
    }

  if (p->event_drul_[STOP])
    {
      assert (!p->finished_bracket_);

      Grob *cmc = unsmob_grob (get_property ("currentMusicalColumn"));

      if (!p->bracket_->get_bound (RIGHT))
	p->bracket_->set_bound (RIGHT, cmc);

      /*
	Set properties so that the stencil-creating function will
	know whether the right edge should be flared ___/
      */

      if (!p->event_drul_[START])
	{
	  SCM flare = p->bracket_->get_property ("bracket-flare");
	  if (scm_is_pair (flare))
	    p->bracket_->set_property ("bracket-flare", scm_cons (scm_car (flare),
								  scm_from_double (0)));
	}

      p->finished_bracket_ = p->bracket_;
      p->bracket_ = 0;

      announce_end_grob (p->finished_bracket_, p->event_drul_[STOP]->self_scm ());
      
      p->current_bracket_ev_ = 0;
    }

  if (p->event_drul_[START])
    {
      p->start_ev_ = p->event_drul_[START];
      p->current_bracket_ev_ = p->event_drul_[START];

      p->bracket_ = make_spanner ("PianoPedalBracket", p->event_drul_[START]->self_scm ());

      /*
	Set properties so that the stencil-creating function will
	know whether the left edge should be flared \___
      */

      if (!p->finished_bracket_)
	{
	  SCM flare = p->bracket_->get_property ("bracket-flare");
	  p->bracket_->set_property ("bracket-flare", scm_cons (scm_from_double (0), scm_cdr (flare)));
	}

      /* Set this property for 'mixed style' pedals,    Ped._______/\ ,
	 so the stencil function will shorten the ____ line by the length of the Ped. text.
      */

      if (mixed)
	{
	  /*
	    Mixed style: Store a pointer to the preceding text for use in
	    calculating the length of the line


	    TODO:

	    WTF is pedal-text not the bound of the object? --hwn
	  */
	  if (p->item_)
	    p->bracket_->set_object ("pedal-text", p->item_->self_scm ());
	}
    }

  p->event_drul_[START] = 0;
  p->event_drul_[STOP] = 0;
}

void
Piano_pedal_engraver::finalize ()
{
  for (Pedal_info *p = info_list_; p->type_; p++)
    {
      if (p->bracket_
	  && !p->bracket_->is_live ())
	p->bracket_ = 0;

      if (p->bracket_)
	{
	  SCM cc = get_property ("currentCommandColumn");
	  Item *c = unsmob_item (cc);
	  p->bracket_->set_bound (RIGHT, c);

	  p->finished_bracket_ = p->bracket_;
	  p->bracket_ = 0;
	  typeset_all (p);
	}

    }
}

void
Piano_pedal_engraver::stop_translation_timestep ()
{
  for (Pedal_info *p = info_list_; p->type_; p++)
    {
      
      typeset_all (p);
      if (p->bracket_ && !p->bracket_->get_bound (LEFT))
	{
	  Grob *cmc = unsmob_grob (get_property ("currentMusicalColumn"));

	  if (!p->bracket_->get_bound (LEFT))
	    p->bracket_->set_bound (LEFT, cmc);
	}
    }

  for (Pedal_info *p = info_list_; p->type_; p++)
    {
      p->event_drul_[STOP] = 0;
      p->event_drul_[START] = 0;
    }
}

void
Piano_pedal_engraver::typeset_all (Pedal_info *p)
{
  /*
    Handle suicide.
  */
  if (p->finished_bracket_
      && !p->finished_bracket_->is_live ())
    p->finished_bracket_ = 0;

  if (p->item_)
    p->item_ = 0;

  if (p->finished_bracket_)
    {
      Grob *r = p->finished_bracket_->get_bound (RIGHT);
      if (!r)
	p->finished_bracket_->set_bound (RIGHT, unsmob_grob (get_property ("currentMusicalColumn")));

      p->finished_bracket_ = 0;
    }
}

ADD_ACKNOWLEDGER (Piano_pedal_engraver, note_column);

ADD_TRANSLATOR (Piano_pedal_engraver,
		/* doc */
		"Engrave piano pedal symbols and brackets.",

		/* create */
		"PianoPedalBracket "
		"SostenutoPedal "
		"SustainPedal "
		"UnaCordaPedal ",

		/* read */
		"currentCommandColumn "
		"pedalSostenutoStrings "
		"pedalSostenutoStyle "
		"pedalSustainStrings "
		"pedalSustainStyle "
		"pedalUnaCordaStrings "
		"pedalUnaCordaStyle ",
		
		/* write */
		""
		);
