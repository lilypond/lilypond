/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "axis-group-interface.hh"
#include "hara-kiri-group-spanner.hh"
#include "pointer-group-interface.hh"
#include "context.hh"
#include "international.hh"
#include "spanner.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

/**
   Put stuff in a Spanner with an Axis_group_interface.
   Use as last element of a context.
*/
class Axis_group_engraver : public Engraver
{
protected:
  bool active_;
  Spanner *staffline_;
  SCM interesting_;
  vector<Grob *> elts_;
  void process_music ();
  void initialize () override;
  void finalize () override;
  void acknowledge_grob (Grob_info) override;
  void process_acknowledged ();
  virtual Spanner *get_spanner ();
  virtual void add_element (Grob *);
  bool must_be_last () const override;
  void derived_mark () const override;

public:
  TRANSLATOR_DECLARATIONS (Axis_group_engraver);
};

Axis_group_engraver::Axis_group_engraver (Context *c)
  : Engraver (c)
{
  staffline_ = 0;
  interesting_ = SCM_EOL;
  active_ = false;
}

void
Axis_group_engraver::initialize ()
{
  active_ = !from_scm<bool> (get_property (this, "hasAxisGroup"));
  if (active_)
    set_property (context (), "hasAxisGroup", SCM_BOOL_T);
}

void
Axis_group_engraver::derived_mark () const
{
  scm_gc_mark (interesting_);
}

bool
Axis_group_engraver::must_be_last () const
{
  return true;
}

void
Axis_group_engraver::process_music ()
{
  if (!staffline_ && active_)
    {
      staffline_ = get_spanner ();
      Grob *it = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      staffline_->set_bound (LEFT, it);
    }
  interesting_ = get_property (this, "keepAliveInterfaces");
}

Spanner *
Axis_group_engraver::get_spanner ()
{
  return make_spanner ("VerticalAxisGroup", SCM_EOL);
}

void
Axis_group_engraver::finalize ()
{
  if (staffline_)
    {
      Grob *it = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      staffline_->set_bound (RIGHT, it);

      Pointer_group_interface::set_ordered (staffline_,
                                            ly_symbol2scm ("elements"), false);
    }
}

void
Axis_group_engraver::acknowledge_grob (Grob_info i)
{
  if (!staffline_)
    return;

  elts_.push_back (i.grob ());

  if (from_scm<bool> (get_property (staffline_, "remove-empty")))
    {
      for (SCM s = interesting_; scm_is_pair (s); s = scm_cdr (s))
        {
          if (i.grob ()->internal_has_interface (scm_car (s)))
            {
              Hara_kiri_group_spanner::add_interesting_item (staffline_,
                                                             i.grob ());
              break;
            }
        }
    }
}

/*
  maybe should check if our parent is set, because we now get a
  cyclic parent relationship if we have two Axis_group_engravers in
  the context.  */
void
Axis_group_engraver::process_acknowledged ()
{
  if (!staffline_)
    return;

  for (vsize i = 0; i < elts_.size (); i++)
    {
      if (!unsmob<Grob> (get_object (elts_[i], "axis-group-parent-Y")))
        {
          if (staffline_->get_y_parent ()
              && staffline_->get_y_parent () == elts_[i])
            {
              staffline_->warning (
                _ ("Axis_group_engraver: vertical group already has a parent"));
              staffline_->warning (_ ("are there two Axis_group_engravers?"));
              staffline_->warning (_ ("removing this vertical group"));
              staffline_->suicide ();
              staffline_ = 0;
              break;
            }
          add_element (elts_[i]);
        }
    }
  elts_.clear ();
}

void
Axis_group_engraver::add_element (Grob *e)
{
  Axis_group_interface::add_element (staffline_, e);
}

void
Axis_group_engraver::boot ()
{
  ADD_ACKNOWLEDGER (grob);
}

ADD_TRANSLATOR (Axis_group_engraver,
                /* doc */
                R"(
Group all objects created in this context in a @code{VerticalAxisGroup}
spanner.
                )",

                /* create */
                R"(
VerticalAxisGroup
                )",

                /* read */
                R"(
currentCommandColumn
keepAliveInterfaces
hasAxisGroup
                )",

                /* write */
                R"(
hasAxisGroup
                )");
