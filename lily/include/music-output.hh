/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef MUSIC_OUTPUT_HH
#define MUSIC_OUTPUT_HH

#include "std-string.hh"
#include "lily-proto.hh"
#include "protected-scm.hh"
#include "smobs.hh"
#include "virtual-methods.hh"

class Music_output : public Smob<Music_output>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Music_output ();

private:
  VIRTUAL_CLASS_NAME (Music_output);

protected:
  Music_output ();

public:
  virtual void derived_mark () const;
  virtual void process ();
};

#endif /* MUSIC_OUTPUT_HH */
