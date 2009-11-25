/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grob-info.hh"
#include "translator.hh"

/**
   a struct which processes events, and creates the #Grob#s.
   It may use derived classes.
*/
class Engraver : public Translator
{
  Grob *internal_make_grob (SCM sym, SCM cause, char const *name,
			    char const *f, int l, char const *fun);

  friend class Engraver_group;
protected:
  /*
    take note of item/spanner
    put item in spanner. Adjust local key; etc.

    Default: ignore the info
  */
  virtual void acknowledge_grob (Grob_info) {}
  virtual void announce_grob (Grob_info);
  virtual void announce_end_grob (Grob_info);
  Engraver_group *get_daddy_engraver () const;

public:
  /**
     Announce element. Default: pass on to daddy. Utility
  */
  void announce_grob (Grob *, SCM cause);
  void announce_end_grob (Grob *, SCM cause);

  Item *internal_make_item (SCM sym, SCM cause, char const *name,
			    char const *f, int l, char const *fun);
  Spanner *internal_make_spanner (SCM sym, SCM cause, char const *name,
				  char const *f, int l, char const *fun);
  Paper_column *internal_make_column (SCM sym, char const *name,
				      char const *f, int l, char const *fun);

  /**
     override other ctor
  */
  TRANSLATOR_DECLARATIONS (Engraver);
};

#define make_item(x, cause) internal_make_item (ly_symbol2scm (x), cause, x, __FILE__, __LINE__, __FUNCTION__)
#define make_spanner(x, cause) internal_make_spanner (ly_symbol2scm (x), cause, x, __FILE__, __LINE__, __FUNCTION__)
#define make_paper_column(x) internal_make_column (ly_symbol2scm (x), x, __FILE__, __LINE__, __FUNCTION__)


#endif // ENGRAVER_HH
