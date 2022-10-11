/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2013--2022 by Heikki Tauriainen <g034737@welho.com>.
  Adapted from performer implementations
  Copyright (C) 1996--2022 Jan Nieuwenhuizen <janneke@gnu.org>,
  Han-Wen Nienhyus <hanwen@xs4all.nl> and others.

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

#include "performer.hh"

#include "audio-item.hh"
#include "context.hh"
#include "dispatcher.hh"
#include "international.hh"
#include "listener.hh"
#include "midi-cc-announcer.hh"
#include "stream-event.hh"

#include "translator.icc"

using std::string;

/**
   MIDI control change performer.  Announces "set property" events on MIDI
   context properties.
*/
class Midi_control_change_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Midi_control_change_performer);
  void announce_control_change (SCM);
  ~Midi_control_change_performer ();

  void connect_to_context (Context *c) override;
  void disconnect_from_context (Context *c) override;

private:
  class Control_change_announcer : public Midi_control_change_announcer
  {
  public:
    Control_change_announcer (Midi_control_change_performer *p,
                              Stream_event *ev, const string &s);

    SCM get_property_value (const char *property_name) override;
    void do_announce (Audio_control_change *item) override;

  private:
    Midi_control_change_performer *performer_;
    Stream_event *event_;
    string symbol_;
  };
};

Midi_control_change_performer::Midi_control_change_performer (Context *c)
  : Performer (c)
{
}

Midi_control_change_performer::~Midi_control_change_performer ()
{
}

void
Midi_control_change_performer::connect_to_context (Context *c)
{
  c->events_below ()->add_listener (
    GET_LISTENER (this, announce_control_change),
    ly_symbol2scm ("SetProperty"));
}

void
Midi_control_change_performer::disconnect_from_context (Context *c)
{
  c->events_below ()->remove_listener (
    GET_LISTENER (this, announce_control_change),
    ly_symbol2scm ("SetProperty"));
}

void
Midi_control_change_performer::announce_control_change (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);
  SCM sym = get_property (ev, "symbol");
  if (!scm_is_symbol (sym))
    return;

  Control_change_announcer a (this, ev, ly_symbol2string (sym));
  a.announce_control_changes ();
}

Midi_control_change_performer::Control_change_announcer::
  Control_change_announcer (Midi_control_change_performer *p, Stream_event *ev,
                            const string &s)
  : Midi_control_change_announcer (ev->origin ()),
    performer_ (p),
    event_ (ev),
    symbol_ (s)
{
}

SCM
Midi_control_change_performer::Control_change_announcer::get_property_value (
  const char *property_name)
{
  return symbol_ == property_name ? get_property (event_, "value") : SCM_EOL;
}

void
Midi_control_change_performer::Control_change_announcer::do_announce (
  Audio_control_change *item)
{
  performer_->announce_element (Audio_element_info (item, 0));
}

void
Midi_control_change_performer::boot ()
{
}

ADD_TRANSLATOR (Midi_control_change_performer,
                /* doc */
                R"(
This performer listens to SetProperty events on context properties for
generating MIDI control changes and prepares them for MIDI output.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
midiBalance
midiPanPosition
midiExpression
midiReverbLevel
midiChorusLevel
                )",

                /* write */
                R"(

                )");
