/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef AXIS_GROUP_ENGRAVER_HH
#define AXIS_GROUP_ENGRAVER_HH

#include "engraver.hh"

/**
   Put stuff in a Spanner with an Axis_group_interface.
   Use as last element of a context.
*/
class Axis_group_engraver : public Engraver
{
protected:
  Spanner *staffline_;
  vector<Grob*> elts_;
  void process_music ();
  virtual void finalize ();
  DECLARE_ACKNOWLEDGER (grob);
  void process_acknowledged ();
  virtual Spanner *get_spanner ();
  virtual void add_element (Grob *);
  virtual bool must_be_last () const;
  
public:
  TRANSLATOR_DECLARATIONS (Axis_group_engraver);
};
#endif /* AXIS_GROUP_ENGRAVER_HH */
