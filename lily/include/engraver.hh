/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef ENGRAVER_HH
#define ENGRAVER_HH

#include "callback.hh"
#include "grob.hh"
#include "grob-info.hh"
#include "translator.hh"

/**
   a struct which processes events, and creates the #Grob#s.
   It may use derived classes.
*/
class Engraver : public Translator
{
  friend class Engraver_group;

private:
  template <typename T>
  T *choose_grob_type (SCM klass, SCM props);
  template <typename T>
  T *internal_make_grob (SCM sym, SCM cause, char const *f, int l,
                         char const *fun);

protected:
  /*
    take note of item/spanner
    put item in spanner. Adjust local key; etc.

    Default: ignore the info
  */
  virtual void acknowledge_grob (Grob_info) {}
  void announce_grob (Grob_info, Context *reroute_context = 0);
  void announce_end_grob (Grob_info, Context *reroute_context = 0);
  Engraver_group *get_group () const;
  bool is_midi () const override { return false; }

public:
  /**
     Announce element. Default: pass on to daddy. Utility
  */
  void announce_grob (Grob *, SCM cause);
  void announce_end_grob (Grob *, SCM cause);

  Grob_info make_grob_info (Grob *, SCM cause);

  Item *internal_make_item (SCM sym, SCM cause, char const *f, int l,
                            char const *fun);
  Spanner *internal_make_spanner (SCM sym, SCM cause, char const *f, int l,
                                  char const *fun);
  Paper_column *internal_make_column (SCM sym, char const *f, int l,
                                      char const *fun);
  Grob *internal_make_indeterminate (SCM sym, SCM cause, char const *f, int l,
                                     char const *fun);
  Grob *internal_make_sticky (SCM symbol, Grob *host, SCM cause,
                              char const *file, int line, char const *fun);

  /**
     override other ctor
  */
  OVERRIDE_CLASS_NAME (Engraver);
  Engraver (Context *);
};

#define make_item(x, cause)                                                    \
  internal_make_item (ly_symbol2scm (x), cause, __FILE__, __LINE__,            \
                      __FUNCTION__)
#define make_spanner(x, cause)                                                 \
  internal_make_spanner (ly_symbol2scm (x), cause, __FILE__, __LINE__,         \
                         __FUNCTION__)
#define make_paper_column(x)                                                   \
  internal_make_column (ly_symbol2scm (x), __FILE__, __LINE__, __FUNCTION__)
#define make_sticky(x, host, cause)                                            \
  internal_make_sticky (ly_symbol2scm (x), host, cause, __FILE__, __LINE__,    \
                        __FUNCTION__)

bool ly_is_grob_cause (SCM obj);

// Acknowledger trampolines
template <class T, void (T::*callback) (Grob_info_t<Grob>)>
SCM
Callbacks::trampoline (SCM target, SCM grob, SCM source_engraver)
{
  auto *const t = LY_ASSERT_SMOB (T, target, 1);
  auto *const g = LY_ASSERT_SMOB (Grob, grob, 2);
  auto *const e = LY_ASSERT_SMOB (Engraver, source_engraver, 3);

  (t->*callback) ({e, g});
  return SCM_UNSPECIFIED;
}

template <class T, void (T::*callback) (Grob_info_t<Item>)>
SCM
Callbacks::trampoline (SCM target, SCM grob, SCM source_engraver)
{
  auto *const t = LY_ASSERT_SMOB (T, target, 1);
  auto *const g = LY_ASSERT_SMOB (Item, grob, 2);
  auto *const e = LY_ASSERT_SMOB (Engraver, source_engraver, 3);

  (t->*callback) ({e, g});
  return SCM_UNSPECIFIED;
}

template <class T, void (T::*callback) (Grob_info_t<Spanner>)>
SCM
Callbacks::trampoline (SCM target, SCM grob, SCM source_engraver)
{
  auto *const t = LY_ASSERT_SMOB (T, target, 1);
  auto *const g = LY_ASSERT_SMOB (Spanner, grob, 2);
  auto *const e = LY_ASSERT_SMOB (Engraver, source_engraver, 3);

  (t->*callback) ({e, g});
  return SCM_UNSPECIFIED;
}

#endif // ENGRAVER_HH
