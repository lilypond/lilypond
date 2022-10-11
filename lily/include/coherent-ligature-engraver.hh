/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2003--2022 Juergen Reuter <reuter@ipd.uka.de>

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
#ifndef COHERENT_LIGATURE_ENGRAVER_HH
#define COHERENT_LIGATURE_ENGRAVER_HH

#include "ligature-engraver.hh"

#include <vector>

class Coherent_ligature_engraver : public Ligature_engraver
{
public:
  Coherent_ligature_engraver (Context *c)
    : Ligature_engraver (c)
  {
  }
  // no TRANSLATOR_DECLARATIONS (Coherent_ligature_engraver) needed
  // since this class is abstract

protected:
  virtual void build_ligature (Spanner *ligature,
                               std::vector<Item *> const &primitives)
    = 0;

  void typeset_ligature (Spanner *ligature,
                         std::vector<Item *> const &primitives) override;

  virtual void move_related_items_to_column (Item *, Paper_column *, Real);

private:
  void collect_accidentals (Spanner *, std::vector<Item *> const &);
};

#endif // COHERENT_LIGATURE_ENGRAVER_HH
