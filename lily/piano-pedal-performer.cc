/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "piano-pedal.hh"
#include "performer.hh"

#include "audio-item.hh"
#include "international.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

using std::string;
using std::vector;

/**
   perform Piano pedals
*/
class Piano_pedal_performer : public Performer
{
  struct Pedal_info
  {
    Stream_event *start_event_;
    Drul_array<Stream_event *> event_drul_;
  };

public:
  TRANSLATOR_DECLARATIONS (Piano_pedal_performer);

protected:
  void initialize () override;
  void process_music ();
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void listen_sustain (Stream_event *);
  void listen_una_corda (Stream_event *);
  void listen_sostenuto (Stream_event *);

private:
  vector<Audio_piano_pedal *> audios_;
  Pedal_info info_alist_[NUM_PEDAL_TYPES];
};

Piano_pedal_performer::Piano_pedal_performer (Context *c)
  : Performer (c)
{
}

void
Piano_pedal_performer::initialize ()
{
  Pedal_info *p = info_alist_;

  for (int i = 0; i < NUM_PEDAL_TYPES; i++, p++)
    {
      p->event_drul_[START] = 0;
      p->event_drul_[STOP] = 0;
      p->start_event_ = 0;
    }
}

void
Piano_pedal_performer::process_music ()
{
  Pedal_info *p = info_alist_;

  for (int i = 0; i < NUM_PEDAL_TYPES; i++, p++)
    {
      if (p->event_drul_[STOP])
        {
          if (!p->start_event_)
            p->event_drul_[STOP]->warning (
              _ ("cannot find start of piano pedal"));
          else
            {
              Audio_piano_pedal *a = new Audio_piano_pedal;
              a->type_ = static_cast<Pedal_type> (i);
              a->dir_ = STOP;
              audios_.push_back (a);
              Audio_element_info info (a, p->event_drul_[STOP]);
              announce_element (info);
            }
          p->start_event_ = 0;
        }

      if (p->event_drul_[START])
        {
          p->start_event_ = p->event_drul_[START];
          Audio_piano_pedal *a = new Audio_piano_pedal;
          a->type_ = static_cast<Pedal_type> (i);
          a->dir_ = START;
          audios_.push_back (a);
          Audio_element_info info (a, p->event_drul_[START]);
          announce_element (info);
        }
      p->event_drul_[START] = 0;
      p->event_drul_[STOP] = 0;
    }
}

void
Piano_pedal_performer::stop_translation_timestep ()
{
  audios_.clear ();
}

void
Piano_pedal_performer::start_translation_timestep ()
{
  Pedal_info *p = info_alist_;
  for (int i = 0; i < NUM_PEDAL_TYPES; i++, p++)
    {
      p->event_drul_[STOP] = 0;
      p->event_drul_[START] = 0;
    }
}

void
Piano_pedal_performer::listen_sostenuto (Stream_event *r)
{
  Direction d = from_scm<Direction> (get_property (r, "span-direction"));
  info_alist_[SOSTENUTO].event_drul_[d] = r;
}

void
Piano_pedal_performer::listen_sustain (Stream_event *r)
{
  Direction d = from_scm<Direction> (get_property (r, "span-direction"));
  info_alist_[SUSTAIN].event_drul_[d] = r;
}

void
Piano_pedal_performer::listen_una_corda (Stream_event *r)
{
  Direction d = from_scm<Direction> (get_property (r, "span-direction"));
  info_alist_[UNA_CORDA].event_drul_[d] = r;
}

void
Piano_pedal_performer::boot ()
{
  ADD_LISTENER (sostenuto);
  ADD_LISTENER (sustain);
  ADD_LISTENER (una_corda);
}

ADD_TRANSLATOR (Piano_pedal_performer,
                /* doc */
                R"(

                )",

                /* create */
                R"(

                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
