/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2022 Daniel Eble <nine.fierce.ballads@gmail.com>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "engraver.hh"

#include "breathing-sign.hh"
#include "international.hh"
#include "item.hh"
#include "ly-scm-list.hh"
#include "ly-smob-list.hh"
#include "script-interface.hh"
#include "stream-event.hh"

#include "translator.icc"

#include <algorithm>
#include <vector>

class Caesura_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Caesura_engraver);

private:
  void derived_mark () const override;
  void process_music ();
  void process_acknowledged ();
  void stop_translation_timestep ();

  void acknowledge_bar_line (Grob_info_t<Item>);
  void listen_caesura (Stream_event *);

  bool evaluate_caesura_type ();
  Item *choose_x_parent ();
  void make_or_remake_grobs ();

private:
  Stream_event *caesura_ev_ = nullptr;

  // an optional BreathingSign notating the caesura
  Item *breathing_sign_ = nullptr;
  // optional CaesuraScripts
  std::vector<Item *> scripts_;
  // any BarLine observed
  Item *bar_line_ = nullptr;
  // the X-parent of the grobs this engraver has created (if there is one)
  Item *x_parent_ = nullptr;
  bool observations_changed_ = false;

  // cached caesuraType context property value
  SCM caesura_type_ = SCM_EOL;
  // cached caesuraTypeTransform context property value
  SCM caesura_type_transform_ = SCM_EOL;

  // the requested breathing sign type; #f for none
  SCM conf_breathing_sign_type_ = SCM_UNDEFINED;
  // symbol list of the requested script types
  SCM conf_script_types_ = SCM_UNDEFINED;
  // symbol list of allowed articulations
  SCM conf_artic_types_ = SCM_UNDEFINED;

  // symbol list describing the user-provided articulations
  SCM user_artic_types_ = SCM_EOL;
};

Caesura_engraver::Caesura_engraver (Context *c)
  : Engraver (c)
{
}

void
Caesura_engraver::derived_mark () const
{
  scm_gc_mark (caesura_type_);
  scm_gc_mark (caesura_type_transform_);

  scm_gc_mark (conf_breathing_sign_type_);
  scm_gc_mark (conf_script_types_);
  scm_gc_mark (conf_artic_types_);

  scm_gc_mark (user_artic_types_);
}

void
Caesura_engraver::listen_caesura (Stream_event *ev)
{
  assign_event_once (caesura_ev_, ev);
}

void
Caesura_engraver::process_music ()
{
  if (caesura_ev_)
    {
      caesura_type_ = get_property (this, "caesuraType");
      caesura_type_transform_ = get_property (this, "caesuraTypeTransform");

      // Form a symbol list describing the user-provided articulations.
      {
        ly_smob_list<Stream_event> arts (
          get_property (caesura_ev_, "articulations"));
        for (auto *art : arts)
          {
            SCM a_type = get_property (art, "articulation-type");
            if (scm_is_symbol (a_type))
              user_artic_types_ = scm_cons (a_type, user_artic_types_);
          }

        user_artic_types_ = scm_reverse_x (user_artic_types_, SCM_EOL);
      }

      const bool type_changed = evaluate_caesura_type ();
      if (type_changed)
        make_or_remake_grobs ();
    }
}

void
Caesura_engraver::acknowledge_bar_line (Grob_info_t<Item> inf)
{
  if (!caesura_ev_ || bar_line_)
    return;

  auto *const item = inf.grob ();
  if (!item->internal_has_interface (ly_symbol2scm ("span-bar-interface")))
    {
      bar_line_ = item;
      observations_changed_ = true;
    }
}

void
Caesura_engraver::process_acknowledged ()
{
  if (!caesura_ev_)
    return;

  const bool type_changed = observations_changed_ && evaluate_caesura_type ();
  observations_changed_ = false;

  if (type_changed || (x_parent_ != choose_x_parent ()))
    make_or_remake_grobs ();
}

bool
Caesura_engraver::evaluate_caesura_type ()
{
  SCM articulations_sym = ly_symbol2scm ("articulations");

  SCM props = caesura_type_;
  // Add the user's articulations to the caesuraType value.
  props = scm_acons (articulations_sym, user_artic_types_, props);
  // Pass caesuraType through the transform function, if it is set.
  if (ly_is_procedure (caesura_type_transform_))
    {
      ly_scm_list obs;
      if (bar_line_)
        obs.insert_before (obs.begin (), ly_symbol2scm ("bar-line"));

      props = ly_call (caesura_type_transform_, to_scm (context ()), props,
                       obs.begin_scm ());
    }

  SCM b_type = scm_assq_ref (props, ly_symbol2scm ("breath"));
  SCM s_types = ly_assoc_get (ly_symbol2scm ("scripts"), props, SCM_EOL);
  SCM a_types = scm_assq_ref (props, articulations_sym);

  if (ly_is_equal (b_type, conf_breathing_sign_type_)
      && ly_is_equal (s_types, conf_script_types_)
      && ly_is_equal (a_types, conf_artic_types_))
    {
      return false; // no change
    }

  conf_breathing_sign_type_ = b_type;
  conf_script_types_ = s_types;
  conf_artic_types_ = a_types;
  return true;
}

Item *
Caesura_engraver::choose_x_parent ()
{
  // more important because we created it
  if (breathing_sign_)
    return breathing_sign_;

  return bar_line_;
};

void
Caesura_engraver::make_or_remake_grobs ()
{
  SCM articulations_sym = ly_symbol2scm ("articulations");

  const auto &ev = caesura_ev_; // shortens lines below
  assert (ev);

  SCM b_type = conf_breathing_sign_type_;
  SCM s_types = conf_script_types_;
  SCM a_types = conf_artic_types_;

  // Discard the old grobs.

  x_parent_ = nullptr;

  if (breathing_sign_)
    {
      breathing_sign_->suicide ();
      breathing_sign_ = nullptr;
    }

  for (auto *scr : scripts_)
    scr->suicide ();
  scripts_.clear ();

  // Create new grobs.

  if (scm_is_symbol (b_type))
    {
      breathing_sign_ = make_item ("BreathingSign", to_scm (ev));
      Breathing_sign::set_breath_properties (breathing_sign_, context (),
                                             b_type);
    }

  x_parent_ = choose_x_parent ();

  size_t scr_i = 0; // count scripts for make_script_from_event

  for (SCM s_type : as_ly_scm_list (s_types))
    {
      if (!scm_is_symbol (s_type))
        {
          std::string msg ("caesura script type must be a symbol: ");
          msg += ly_scm_write_string (s_type);
          ev->programming_error (msg);
          continue;
        }

      auto *scr = make_item ("CaesuraScript", to_scm (ev));
      make_script_from_event (scr, context (), s_type, scr_i);
      if (x_parent_)
        scr->set_x_parent (x_parent_);

      scripts_.push_back (scr);
      ++scr_i;
    }

  ly_smob_list<Stream_event> arts (get_property (ev, articulations_sym));
  for (auto *art : arts)
    {
      if (!art) // programming_error?
        continue;

      SCM a_type = get_property (art, "articulation-type");

      // caesuraTypeTransform may have narrowed the set of acceptable
      // articulations.  Check whether this one is allowed.
      if (scm_is_false (scm_memq (a_type, a_types)))
        continue;

      auto *scr = make_item ("CaesuraScript", to_scm (art));
      make_script_from_event (scr, context (), a_type, scr_i);
      if (x_parent_)
        scr->set_x_parent (x_parent_);

      // The event may override the default direction of the script.
      SCM dir_sym = ly_symbol2scm ("direction");
      if (auto d = from_scm<Direction> (get_property (art, dir_sym)))
        set_property (scr, dir_sym, to_scm (d));

      scripts_.push_back (scr);
      ++scr_i;
    }
}

void
Caesura_engraver::stop_translation_timestep ()
{
  // Script_column will set the outside-staff-priority of every script after
  // the first, so we set the first.
  if (!scripts_.empty ())
    {
      // To determine which articulation will be first, we must sort them as
      // Script_column will.
      // TODO: There must be a better way -- and this still doesn't match
      // Script_column if some scripts are UP and some are DOWN, though that
      // should not be a problem for traditional caesura engraving.
      std::stable_sort (scripts_.begin (), scripts_.end (),
                        Script_interface::script_priority_less);
      set_property (scripts_.front (), "outside-staff-priority", to_scm (0));
    }

  caesura_ev_ = nullptr;

  breathing_sign_ = nullptr;
  scripts_.clear ();
  bar_line_ = nullptr;
  x_parent_ = nullptr;
  observations_changed_ = false;

  caesura_type_ = SCM_EOL;
  caesura_type_transform_ = SCM_EOL;

  conf_breathing_sign_type_ = SCM_UNDEFINED;
  conf_script_types_ = SCM_UNDEFINED;
  conf_artic_types_ = SCM_UNDEFINED;

  user_artic_types_ = SCM_EOL;
}

void
Caesura_engraver::boot ()
{
  ADD_ACKNOWLEDGER (bar_line);
  ADD_LISTENER (caesura);
}

ADD_TRANSLATOR (Caesura_engraver,
                /* doc */
                R"(
Notate a short break in sound that does not shorten the previous note.

Depending on the result of passing the value of @code{caesuraType} through
@code{caesuraTypeTransform}, this engraver may create a @code{BreathingSign}
with @code{CaesuraScript} grobs aligned to it, or it may create
@code{CaesuraScript} grobs and align them to a @code{BarLine}.

If this engraver observes a @code{BarLine}, it calls
@code{caesuraTypeTransform} again with the new information, and if necessary,
recreates its grobs.
                )",

                /* create */
                R"(
BreathingSign
CaesuraScript
                )",

                /* read */
                R"(
breathMarkDefinitions
caesuraType
caesuraTypeTransform
scriptDefinitions
                )",

                /* write */
                R"(

                )");
