/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "context.hh"
#include "score-engraver.hh"
#include "warn.hh"
#include "item.hh"
#include "spanner.hh"

#include "translator.icc"

#include <vector>

/*
  generate bars. Either user ("|:"), or default (new measure)
*/
class Bar_engraver final : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Bar_engraver);

protected:
  void initialize () override;
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_acknowledged ();

  void acknowledge_end_spanner (Grob_info_t<Spanner>);

private:
  Item *bar_ = nullptr;
  std::vector<Spanner *> spanners_;
  bool considered_bar_ = false;
};

Bar_engraver::Bar_engraver (Context *c)
  : Engraver (c)
{
}

void
Bar_engraver::initialize ()
{
  Engraver::initialize ();
  set_property (context (), "currentBarLine", SCM_EOL);
}

void
Bar_engraver::start_translation_timestep ()
{
  // We reset currentBarLine here rather than in stop_translation_timestep ()
  // so that other engravers can use it during stop_translation_timestep ().
  if (bar_)
    {
      bar_ = nullptr;
      set_property (context (), "currentBarLine", SCM_EOL);
    }
}

/*
  Bar_engraver should come *after* any engravers that
  modify whichBar

  This is a little hairy : whichBar may be set by
  Repeat_acknowledge_engraver::process_music, which is at score
  context. This means that grobs could should be created after
  process_music. We do stuff process_acknowledged (), just to be
  on the safe side.
*/

void
Bar_engraver::process_acknowledged ()
{
  // process_acknowledged () can be called more than once.  Whether or not we
  // create a BarLine the first time, we don't want to reconsider it.
  if (!considered_bar_)
    {
      considered_bar_ = true;

      SCM wb = SCM_EOL;
      auto *const wbc = where_defined (context (), "whichBar", &wb);
      if (scm_is_string (wb))
        {
          // Unless the bar type has been forced, map these bar types from
          // their values in the same context in which "whichBar" is defined to
          // the values visible in this engraver's context.  This is inelegant,
          // but it does the trick to support ancient staves without disabling
          // these bar lines in a broader context than necessary.
          //
          // If we can remove enough dependencies on whichBar, we might be able
          // to move the logic of Repeat_acknowledge_engraver into Bar_engraver
          // and not have to perform this mapping.
          if ((wbc != context ())
              && wbc
              && !from_scm<bool> (get_property (wbc, "barForced")))
            {
              if (ly_is_equal (wb, get_property (wbc, "measureBarType")))
                wb = get_property (this, "measureBarType");
              else if (ly_is_equal (wb, get_property (wbc, "sectionBarType")))
                wb = get_property (this, "sectionBarType");
              else if (ly_is_equal (wb, get_property (wbc, "fineBarType")))
                wb = get_property (this, "fineBarType");
              else if (ly_is_equal (wb,
                                    get_property (wbc, "startRepeatBarType")))
                {
                  wb = get_property (this, "startRepeatBarType");
                }
              else if (ly_is_equal (wb,
                                    get_property (wbc, "doubleRepeatBarType")))
                {
                  wb = get_property (this, "doubleRepeatBarType");
                }
              else if (ly_is_equal (wb,
                                    get_property (wbc, "endRepeatBarType")))
                {
                  wb = get_property (this, "endRepeatBarType");
                }
            }

          if (scm_is_string (wb))
            {
              bar_ = make_item ("BarLine", SCM_EOL);
              if (!ly_is_equal (wb, get_property (bar_, "glyph")))
                set_property (bar_, "glyph", wb);

              set_property (context (), "currentBarLine", to_scm (bar_));
            }
        }
    }

  if (bar_)
    {
      for (const auto &sp : spanners_)
        sp->set_bound (RIGHT, bar_);
    }

  spanners_.clear ();
}

/*
  lines may only be broken if there is a barline in all staves
*/
void
Bar_engraver::stop_translation_timestep ()
{
  if (!bar_)
    set_property (find_score_context (), "forbidBreak", SCM_BOOL_T);

  considered_bar_ = false;
}

void
Bar_engraver::acknowledge_end_spanner (Grob_info_t<Spanner> gi)
{
  const bool might_have_bar = !considered_bar_ || bar_;
  if (might_have_bar) // otherwise avoid a little work
    {
      auto *const sp = gi.grob ();
      if (from_scm<bool> (get_property (sp, "to-barline")))
        spanners_.push_back (sp);
    }
}

void
Bar_engraver::boot ()
{
  ADD_END_ACKNOWLEDGER (Bar_engraver, spanner);
}

ADD_TRANSLATOR (Bar_engraver,
                /* doc */
                R"(
Create barlines.  This engraver is controlled through the @code{whichBar}
property.  If it has no bar line to create, it will forbid a linebreak at this
point.  This engraver is required to trigger the creation of clefs at the start
of systems.
                )",

                /* create */
                R"(
BarLine
                )",

                /* read */
                R"(
barForced
doubleRepeatBarType
endRepeatBarType
fineBarType
measureBarType
sectionBarType
startRepeatBarType
whichBar
                )",

                /* write */
                R"(
currentBarLine
forbidBreak
                )");
