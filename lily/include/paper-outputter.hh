/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef PAPER_OUTPUTTER_HH
#define PAPER_OUTPUTTER_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "std-string.hh"
#include "protected-scm.hh"
#include "smobs.hh"

/*
  Glue between the backend (grobs, systems, pages) and the output file.
  proxy for Scheme backends.
*/
class Paper_outputter : public Smob<Paper_outputter>
{
public:
  static const char * const type_p_name_; // = 0
  SCM mark_smob () const;
  virtual ~Paper_outputter ();
private:
  SCM output_module_;
  string file_name_;
  SCM file_;

public:
  Paper_outputter (SCM port, const string &format);

  void close ();
  SCM dump_string (SCM);
  SCM file () const;
  SCM module () const;
  SCM output_scheme (SCM scm);
  void output_stencil (Stencil);
  SCM scheme_to_string (SCM);
};


#endif /* PAPER_OUTPUTTER_HH */
