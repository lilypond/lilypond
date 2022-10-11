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
#ifndef GREGORIAN_LIGATURE_ENGRAVER_HH
#define GREGORIAN_LIGATURE_ENGRAVER_HH

#include "coherent-ligature-engraver.hh"

#include <vector>

class Gregorian_ligature_engraver : public Coherent_ligature_engraver
{
  Stream_event *pes_or_flexa_req_;

public:
  // no TRANSLATOR_DECLARATIONS (Gregorian_ligature_engraver) needed
  // since this class is abstract

protected:
  Gregorian_ligature_engraver (Context *);

  void listen_pes_or_flexa (Stream_event *ev);

  void build_ligature (Spanner *ligature,
                       std::vector<Item *> const &primitives) override;

  virtual void transform_heads (Spanner *ligature,
                                std::vector<Item *> const &primitives)
    = 0;

  void stop_translation_timestep ();
};

#endif // GREGORIAN_LIGATURE_ENGRAVER_HH
