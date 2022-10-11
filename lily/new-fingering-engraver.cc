/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "international.hh"
#include "item.hh"
#include "rhythmic-head.hh"
#include "script-interface.hh"
#include "self-alignment-interface.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

#include <algorithm>
#include <vector>

using std::vector;

struct Finger_tuple
{
  Grob *head_;
  Grob *script_;
  Stream_event *note_event_;
  Stream_event *finger_event_;
  bool follow_into_staff_;
  int position_;

  Finger_tuple ()
  {
    position_ = 0;
    head_ = script_ = 0;
    note_event_ = finger_event_ = 0;
    follow_into_staff_ = false;
  }
};

bool
operator<(Finger_tuple const &a, Finger_tuple const &b)
{
  return a.position_ < b.position_;
}

class New_fingering_engraver : public Engraver
{
  vector<Finger_tuple> fingerings_;
  vector<Finger_tuple> stroke_fingerings_;
  vector<Finger_tuple> articulations_;
  vector<Finger_tuple> string_numbers_;

  vector<Grob *> heads_;
  vector<Grob *> accidentals_;
  Grob *stem_;
  Item *note_column_;

  void position_all ();

public:
  TRANSLATOR_DECLARATIONS (New_fingering_engraver);

protected:
  void stop_translation_timestep ();
  void acknowledge_rhythmic_head (Grob_info);
  void acknowledge_inline_accidental (Grob_info);
  void acknowledge_stem (Grob_info);
  void acknowledge_note_column (Grob_info_t<Item>);
  void add_fingering (Grob *, SCM, vector<Finger_tuple> *, Stream_event *,
                      Stream_event *);
  void add_script (Grob *, Stream_event *, Stream_event *);
  void position_scripts (SCM orientations, vector<Finger_tuple> *);
};

void
New_fingering_engraver::acknowledge_inline_accidental (Grob_info inf)
{
  accidentals_.push_back (inf.grob ());
}

void
New_fingering_engraver::acknowledge_rhythmic_head (Grob_info inf)
{
  Stream_event *note_ev = inf.event_cause ();
  if (!note_ev)
    return;

  SCM arts = get_property (note_ev, "articulations");

  for (SCM s = arts; scm_is_pair (s); s = scm_cdr (s))
    {
      Stream_event *ev = unsmob<Stream_event> (scm_car (s));

      if (!ev)
        continue;

      if (ev->in_event_class ("fingering-event"))
        add_fingering (inf.grob (), ly_symbol2scm ("Fingering"), &fingerings_,
                       ev, note_ev);
      else if (ev->in_event_class ("text-script-event"))
        ev->warning (_ ("cannot add text scripts to individual note heads"));
      else if (ev->in_event_class ("script-event"))
        add_script (inf.grob (), ev, note_ev);
      else if (ev->in_event_class ("string-number-event"))
        {
          add_fingering (inf.grob (), ly_symbol2scm ("StringNumber"),
                         &string_numbers_, ev, note_ev);
        }
      else if (ev->in_event_class ("stroke-finger-event"))
        add_fingering (inf.grob (), ly_symbol2scm ("StrokeFinger"),
                       &stroke_fingerings_, ev, note_ev);
      else if (ev->in_event_class ("harmonic-event"))
        {
          set_property (inf.grob (), "style", ly_symbol2scm ("harmonic"));
          Grob *d = unsmob<Grob> (get_object (inf.grob (), "dot"));
          if (d && !from_scm<bool> (get_property (this, "harmonicDots")))
            d->suicide ();
        }
    }

  heads_.push_back (inf.grob ());
}

void
New_fingering_engraver::acknowledge_stem (Grob_info inf)
{
  stem_ = inf.grob ();
}

void
New_fingering_engraver::acknowledge_note_column (Grob_info_t<Item> inf)
{
  note_column_ = inf.grob ();
}

void
New_fingering_engraver::add_script (Grob *head, Stream_event *event,
                                    Stream_event * /* note */)
{
  Finger_tuple ft;

  Grob *g = make_item ("Script", event->self_scm ());
  make_script_from_event (g, context (),
                          get_property (event, "articulation-type"), 0);
  ft.script_ = g;
  ft.script_->set_x_parent (head);

  SCM forced_dir = get_property (event, "direction");
  if (from_scm<Direction> (forced_dir))
    set_property (ft.script_, "direction", forced_dir);

  articulations_.push_back (ft);
}

void
New_fingering_engraver::add_fingering (Grob *head, SCM grob_sym,
                                       vector<Finger_tuple> *tuple_vector,
                                       Stream_event *event,
                                       Stream_event *hevent)
{
  Finger_tuple ft;

  ft.script_ = make_item (grob_sym, event->self_scm ());

  Side_position_interface::add_support (ft.script_, head);

  ft.finger_event_ = event;
  ft.note_event_ = hevent;
  ft.head_ = head;

  tuple_vector->push_back (ft);
}

void
New_fingering_engraver::position_scripts (SCM orientations,
                                          vector<Finger_tuple> *scripts)
{
  for (vsize i = 0; i < scripts->size (); i++)
    if (stem_)
      {
        Side_position_interface::add_support (scripts->at (i).script_, stem_);
        if (Grob *flag = unsmob<Grob> (get_object (stem_, "flag")))
          Side_position_interface::add_support (scripts->at (i).script_, flag);
      }

  /*
    This is not extremely elegant, but we have to do a little
    formatting here, because the parent/child relations should be
    known before we move on to the next time step.

    A more sophisticated approach would be to set both X and Y parents
    to the note head, and write a more flexible function for
    positioning the fingerings, setting both X and Y coordinates.
  */
  for (vsize i = 0; i < scripts->size (); i++)
    (*scripts)[i].position_
      = from_scm<int> (get_property ((*scripts)[i].head_, "staff-position"));

  for (vsize i = scripts->size (); i--;)
    for (vsize j = heads_.size (); j--;)
      Side_position_interface::add_support ((*scripts)[i].script_, heads_[j]);

  vector<Finger_tuple> up, down, horiz;
  for (vsize i = scripts->size (); i--;)
    {
      SCM d = get_property ((*scripts)[i].finger_event_, "direction");
      if (from_scm<Direction> (d))
        {
          ((from_scm<Direction> (d) == UP) ? up : down)
            .push_back ((*scripts)[i]);
          scripts->erase (scripts->begin () + i);
        }
    }

  std::sort (scripts->begin (), scripts->end ());

  bool up_p = scm_is_true (scm_c_memq (ly_symbol2scm ("up"), orientations));
  bool down_p = scm_is_true (scm_c_memq (ly_symbol2scm ("down"), orientations));
  bool left_p = scm_is_true (scm_c_memq (ly_symbol2scm ("left"), orientations));
  bool right_p
    = scm_is_true (scm_c_memq (ly_symbol2scm ("right"), orientations));
  Direction hordir = (right_p) ? RIGHT : LEFT;
  if (left_p || right_p)
    {
      if (up_p && !up.size () && scripts->size ())
        {
          up.push_back (scripts->back ());
          scripts->pop_back ();
        }

      if (down_p && !down.size () && scripts->size ())
        {
          down.push_back ((*scripts)[0]);
          scripts->erase (scripts->begin ());
        }

      horiz.insert (horiz.end (), scripts->begin (), scripts->end ());
    }
  else if (up_p && down_p)
    {
      vsize center = scripts->size () / 2;
      down.insert (down.end (), scripts->begin (), scripts->begin () + center);
      up.insert (up.end (), scripts->begin () + center, scripts->end ());
    }
  else if (up_p)
    {
      up.insert (up.end (), scripts->begin (), scripts->end ());
      scripts->clear ();
    }
  else
    {
      if (!down_p)
        {
          warning (_ ("no placement found for fingerings"));
          warning (_ ("placing below"));
        }
      down.insert (down.end (), scripts->begin (), scripts->end ());
      scripts->clear ();
    }

  for (vsize i = 0; i < horiz.size (); i++)
    {
      Finger_tuple ft = horiz[i];
      Grob *f = ft.script_;
      f->set_x_parent (ft.head_);
      f->set_y_parent (ft.head_);
      set_property (f, "avoid-slur", ly_symbol2scm ("inside"));
      if (hordir == LEFT
          && unsmob<Grob> (get_object (ft.head_, "accidental-grob")))
        Side_position_interface::add_support (
          f, unsmob<Grob> (get_object (ft.head_, "accidental-grob")));
      else if (Rhythmic_head::dot_count (ft.head_))
        for (vsize j = 0; j < heads_.size (); j++)
          if (Grob *d = unsmob<Grob> (get_object (heads_[j], "dot")))
            Side_position_interface::add_support (f, d);

      if (horiz.size () > 1) /* -> FingeringColumn */
        {
          // Ouch, should do this in the typesetting phase. --JeanAS
          const Stencil *stil = f->get_stencil ();
          if (stil)
            {
              Stencil aligned = *stil;
              aligned.align_to (Y_AXIS, CENTER);
              set_property (f, "stencil", aligned.smobbed_copy ());
            }
        }
      else
        {
          auto self_align_y
            = Self_alignment_interface::aligned_on_parent (f, Y_AXIS);
          SCM yoff = get_property (f, "Y-offset");
          if (scm_is_number (yoff))
            self_align_y = self_align_y + from_scm<double> (yoff);
          set_property (f, "Y-offset", to_scm (self_align_y));
        }

      Side_position_interface::set_axis (f, X_AXIS);

      set_property (f, "direction", to_scm (hordir));
    }

  Drul_array<vector<Finger_tuple>> vertical (down, up);
  for (const auto d : {DOWN, UP})
    {
      for (vsize i = 0; i < vertical[d].size (); i++)
        {
          Finger_tuple ft = vertical[d][i];
          Grob *f = ft.script_;
          int finger_prio = from_scm (get_property (f, "script-priority"), 200);

          if (heads_.size () > 1
              && from_scm<bool> (get_property (f, "X-align-on-main-noteheads")))
            f->set_x_parent (note_column_);
          else
            {
              f->set_x_parent (ft.head_);
              if (heads_.size () > 1)
                for (vsize j = 0; j < accidentals_.size (); j++)
                  Side_position_interface::add_support (f, accidentals_[j]);
            }

          set_property (f, "script-priority",
                        to_scm (finger_prio + d * ft.position_));

          Self_alignment_interface::set_aligned_on_parent (f, X_AXIS);
          Side_position_interface::set_axis (f, Y_AXIS);

          set_property (f, "direction", to_scm (d));
        }
    }
}

void
New_fingering_engraver::stop_translation_timestep ()
{
  position_all ();
  stem_ = 0;
  note_column_ = 0;
  heads_.clear ();
  accidentals_.clear ();
}

void
New_fingering_engraver::position_all ()
{
  if (fingerings_.size ())
    {
      position_scripts (get_property (this, "fingeringOrientations"),
                        &fingerings_);
      fingerings_.clear ();
    }

  if (string_numbers_.size ())
    {
      position_scripts (get_property (this, "stringNumberOrientations"),
                        &string_numbers_);
      string_numbers_.clear ();
    }

  if (stroke_fingerings_.size ())
    {
      position_scripts (get_property (this, "strokeFingerOrientations"),
                        &stroke_fingerings_);
      stroke_fingerings_.clear ();
    }

  for (vsize i = articulations_.size (); i--;)
    {
      Grob *script = articulations_[i].script_;
      for (vsize j = 0; j < accidentals_.size (); j++)
        Side_position_interface::add_support (script, accidentals_[j]);

      accidentals_.resize (0);
      for (vsize j = heads_.size (); j--;)
        Side_position_interface::add_support (script, heads_[j]);

      if (stem_
          && from_scm<Direction> (
            get_property (script, "side-relative-direction")))
        set_object (script, "direction-source", stem_->self_scm ());

      if (stem_ && from_scm<bool> (get_property (script, "add-stem-support")))
        Side_position_interface::add_support (script, stem_);
    }
  articulations_.clear ();
}

New_fingering_engraver::New_fingering_engraver (Context *c)
  : Engraver (c)
{
  stem_ = 0;
}

void
New_fingering_engraver::boot ()
{
  ADD_ACKNOWLEDGER (rhythmic_head);
  ADD_ACKNOWLEDGER (inline_accidental);
  ADD_ACKNOWLEDGER (stem);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (New_fingering_engraver,
                /* doc */
                R"(
Create fingering scripts for notes in a new chord.  This engraver is ill-named,
since it also takes care of articulations and harmonic note heads.
                )",

                /* create */
                R"(
Fingering
StringNumber
StrokeFinger
Script
                )",

                /* read */
                R"(
fingeringOrientations
harmonicDots
strokeFingerOrientations
stringNumberOrientations
                )",

                /* write */
                R"(

                )");
