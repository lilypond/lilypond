/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "score-performer.hh"

#include "audio-column.hh"
#include "audio-item.hh"
#include "context-def.hh"
#include "context.hh"
#include "dispatcher.hh"
#include "performance.hh"
#include "midi-stream.hh"
#include "output-def.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "audio-staff.hh"

ADD_TRANSLATOR_GROUP (Score_performer,
                      /* doc */
                      "",

                      /* create */
                      "",

                      /* read */
                      "",

                      /* write */
                      ""
                     );

Score_performer::Score_performer ()
{
  performance_ = 0;
  skipping_ = false;
  audio_column_ = 0;
}

Score_performer::~Score_performer ()
{
}

void
Score_performer::announce_element (Audio_element_info info)
{
  announce_infos_.push_back (info);
  if (Audio_staff *s = dynamic_cast<Audio_staff *> (info.elem_))
    {
      performance_->audio_staffs_.push_back (s);
    }

  performance_->add_element (info.elem_);
}

void
Score_performer::acknowledge_audio_elements ()
{
  for (vsize i = 0; i < announce_infos_.size (); i++)
    {
      if (Audio_item *ai = dynamic_cast<Audio_item *> (announce_infos_[i].elem_))
        audio_column_->add_audio_item (ai);
    }
  Performer_group::acknowledge_audio_elements ();
}

void
Score_performer::connect_to_context (Context *c)
{
  Performer_group::connect_to_context (c);

  Dispatcher *d = find_top_context (c)->event_source ();
  d->add_listener (GET_LISTENER (Score_performer, one_time_step), ly_symbol2scm ("OneTimeStep"));
  d->add_listener (GET_LISTENER (Score_performer, prepare), ly_symbol2scm ("Prepare"));
  d->add_listener (GET_LISTENER (Score_performer, finish), ly_symbol2scm ("Finish"));
}

void
Score_performer::disconnect_from_context ()
{
  Dispatcher *d = find_top_context (context ())->event_source ();
  d->remove_listener (GET_LISTENER (Score_performer, one_time_step), ly_symbol2scm ("OneTimeStep"));
  d->remove_listener (GET_LISTENER (Score_performer, prepare), ly_symbol2scm ("Prepare"));
  d->remove_listener (GET_LISTENER (Score_performer, finish), ly_symbol2scm ("Finish"));

  Performer_group::disconnect_from_context ();
}

void
Score_performer::prepare (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);
  SCM sm = ev->get_property ("moment");
  Moment *m = unsmob<Moment> (sm);
  audio_column_ = new Audio_column (*m);
  announce_element (Audio_element_info (audio_column_, 0));
  precomputed_recurse_over_translators (context (), START_TRANSLATION_TIMESTEP, UP);
}

void
Score_performer::finish (SCM)
{
  SCM channel_mapping = context ()->get_property ("midiChannelMapping");
  bool use_ports = scm_is_eq (channel_mapping, ly_symbol2scm ("voice"));
  performance_->ports_ = use_ports;
  recurse_over_translators
  (context (),
   MFP0_WRAP (&Translator::finalize),
   MFP0_WRAP (&Translator_group::finalize),
   UP);
}

void
Score_performer::one_time_step (SCM)
{
  // audio_column_ can be 0 when prepare has not been called.  The
  // condition is triggered when Simple_music_iterator implicitly
  // creates a Score context, like when writing
  //
  // \score { { | c4 c c c } \midi { } }
  //
  // The same situation happens with the Score_engraver group, but it
  // would appear not to suffer any bad side effects.

  if (!audio_column_)
    audio_column_ = new Audio_column (context ()->now_mom ());
  if (to_boolean (context ()->get_property ("skipTypesetting")))
    {
      if (!skipping_)
        {
          skip_start_mom_ = audio_column_->when ();
          skipping_ = true;
        }
    }
  else
    {
      if (skipping_)
        {
          offset_mom_ -= audio_column_->when () - skip_start_mom_;
          skipping_ = false;
        }

      audio_column_->offset_when (offset_mom_);
      precomputed_recurse_over_translators (context (), PROCESS_MUSIC, UP);
      do_announces ();
    }

  precomputed_recurse_over_translators (context (), STOP_TRANSLATION_TIMESTEP, UP);
}

void
Score_performer::derived_mark () const
{
  if (performance_)
    scm_gc_mark (performance_->self_scm ());

  Performer_group::derived_mark ();
}

void
Score_performer::initialize ()
{
  performance_ = new Performance;
  performance_->unprotect ();
  context ()->set_property ("output", performance_->self_scm ());
  performance_->midi_ = context ()->get_output_def ();

  Translator_group::initialize ();
}
