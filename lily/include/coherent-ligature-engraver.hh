/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2003--2009 Juergen Reuter <reuter@ipd.uka.de>

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

class Coherent_ligature_engraver : public Ligature_engraver
{
public:
  // no TRANSLATOR_DECLARATIONS (Coherent_ligature_engraver) needed
  // since this class is abstract

protected:
  virtual void build_ligature (Spanner *ligature,
			       vector<Grob_info> primitives) = 0;
  virtual void typeset_ligature (Spanner *ligature,
				 vector<Grob_info> primitives);
  virtual void move_related_items_to_column (Item *, Paper_column *, Real);
private:
  void collect_accidentals (Spanner *, vector<Grob_info>);
};

#endif // COHERENT_LIGATURE_ENGRAVER_HH
