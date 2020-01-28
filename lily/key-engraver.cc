/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "clef.hh"
#include "context.hh"
#include "engraver.hh"
#include "international.hh"
#include "item.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"

#include "translator.icc"

class Key_engraver : public Engraver
{
  void create_key (bool);
  void read_event (Stream_event const *r);

  Stream_event *key_event_;
  Item *item_;
  Item *cancellation_;

public:
  TRANSLATOR_DECLARATIONS (Key_engraver);

protected:
  void initialize () override;
  void finalize () override;
  void stop_translation_timestep ();
  void process_music ();

  void listen_key_change (Stream_event *);
  void acknowledge_clef (Grob_info);
  void acknowledge_bar_line (Grob_info);
};

void
Key_engraver::finalize ()
{
}

Key_engraver::Key_engraver (Context *c) : Engraver (c)
{
  key_event_ = 0;
  item_ = 0;
  cancellation_ = 0;
}

void
Key_engraver::create_key (bool is_default)
{
  if (!item_)
    {
      item_ = make_item ("KeySignature",
                         key_event_ ? key_event_->self_scm () : SCM_EOL);

      /* Use middleCClefPosition rather than middleCPosition, because cue
       * notes with a different clef will modify middleCPosition. The
       * Key signature, however, should still be printed at the original
       * position. */
      item_->set_property ("c0-position", get_property ("middleCClefPosition"));

      SCM last = get_property ("lastKeyAlterations");
      SCM key = get_property ("keyAlterations");

      if ((to_boolean (get_property ("printKeyCancellation"))
           || scm_is_null (key))
          && !scm_is_eq (last, key))
        {
          SCM restore = SCM_EOL;
          for (SCM s = last; scm_is_pair (s); s = scm_cdr (s))
            {
              SCM new_alter_pair = scm_assoc (scm_caar (s), key);
              Rational old_alter = robust_scm2rational (scm_cdar (s), 0);
              if (scm_is_false (new_alter_pair)
                  || ((ly_scm2rational (scm_cdr (new_alter_pair)) - old_alter)
                          * old_alter
                      < Rational (0)))
                {
                  restore = scm_cons (scm_car (s), restore);
                }
            }

          if (scm_is_pair (restore))
            {
              cancellation_
                  = make_item ("KeyCancellation",
                               key_event_ ? key_event_->self_scm () : SCM_EOL);

              cancellation_->set_property ("alteration-alist", restore);
              cancellation_->set_property (
                  "c0-position", get_property ("middleCClefPosition"));
            }
        }

      item_->set_property ("alteration-alist", scm_reverse (key));
    }

  if (!is_default)
    {
      SCM visibility = get_property ("explicitKeySignatureVisibility");
      item_->set_property ("break-visibility", visibility);
      item_->set_property ("non-default", SCM_BOOL_T);
    }
}

void
Key_engraver::listen_key_change (Stream_event *ev)
{
  /* do this only once, just to be on the safe side.  */
  if (ASSIGN_EVENT_ONCE (key_event_, ev))
    read_event (key_event_);
}

void Key_engraver::acknowledge_clef (Grob_info /* info */)
{
  SCM c = get_property ("createKeyOnClefChange");
  if (to_boolean (c))
    create_key (false);
}

void Key_engraver::acknowledge_bar_line (Grob_info /* info */)
{
  create_key (true);
}

void
Key_engraver::process_music ()
{
  if (key_event_
      || !scm_is_eq (get_property ("lastKeyAlterations"),
                     get_property ("keyAlterations")))
    create_key (false);
}

void
Key_engraver::stop_translation_timestep ()
{
  item_ = 0;
  context ()->set_property ("lastKeyAlterations",
                            get_property ("keyAlterations"));
  cancellation_ = 0;
  key_event_ = 0;
}

void
Key_engraver::read_event (Stream_event const *r)
{
  SCM p = r->get_property ("pitch-alist");
  if (!scm_is_pair (p))
    return;

  SCM accs = SCM_EOL;

  SCM alist = scm_list_copy (p);
  SCM order = get_property ("keyAlterationOrder");
  for (SCM s = order; scm_is_pair (s) && scm_is_pair (alist); s = scm_cdr (s))
    {
      SCM head = scm_member (scm_car (s), alist);

      if (scm_is_pair (head))
        {
          accs = scm_cons (scm_car (head), accs);
          alist = scm_delete_x (scm_car (head), alist);
        }
    }

  if (scm_is_pair (alist))
    {
      bool warn = false;
      for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
        if (ly_scm2rational (scm_cdar (s)))
          {
            warn = true;
            accs = scm_cons (scm_car (s), accs);
          }

      if (warn)
        r->origin ()->warning (
            _ ("Incomplete keyAlterationOrder for key signature"));
    }

  context ()->set_property ("keyAlterations", scm_reverse_x (accs, SCM_EOL));
  context ()->set_property ("tonic", r->get_property ("tonic"));
}

void
Key_engraver::initialize ()
{
  context ()->set_property ("keyAlterations", SCM_EOL);
  context ()->set_property ("lastKeyAlterations", SCM_EOL);

  Pitch p;
  context ()->set_property ("tonic", p.smobbed_copy ());
}

void
Key_engraver::boot ()
{
  ADD_LISTENER (Key_engraver, key_change);
  ADD_ACKNOWLEDGER (Key_engraver, clef);
  ADD_ACKNOWLEDGER (Key_engraver, bar_line);
}

ADD_TRANSLATOR (Key_engraver,
                /* doc */
                "Engrave a key signature.",

                /* create */
                "KeyCancellation "
                "KeySignature ",

                /* read */
                "createKeyOnClefChange "
                "explicitKeySignatureVisibility "
                "extraNatural "
                "keyAlterationOrder "
                "keyAlterations "
                "lastKeyAlterations "
                "printKeyCancellation "
                "middleCClefPosition ",

                /* write */
                "keyAlterations "
                "lastKeyAlterations "
                "tonic ");
