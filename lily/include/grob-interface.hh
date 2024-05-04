/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2023 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef GROB_INTERFACE_HH
#define GROB_INTERFACE_HH

#include "lily-guile.hh"
#include "protected-scm.hh"

class Grob;

#define ADD_INTERFACE(cl, b, c)                                                \
  template <>                                                                  \
  Grob_interface<cl> Grob_interface<cl>::instance_                             \
    = (add_scm_init_func (                                                     \
         [] { Grob_interface<cl>::instance_.add_interface (#cl, b, c); }),     \
       Grob_interface<cl> ())

SCM ly_add_interface (SCM, SCM, SCM);
void internal_add_interface (SCM, SCM, SCM);
SCM ly_all_grob_interfaces ();

class Grob_interface_base
{
protected:
  Protected_scm interface_symbol_;

protected:
  void add_interface (char const *cxx_name, char const *descr,
                      char const *vars);
};

template <class Interface>
class Grob_interface : private Grob_interface_base
{
public:
  static SCM symbol_scm () { return instance_.interface_symbol_; }

private:
  Grob_interface () = default;

private:
  static Grob_interface instance_;
};

#endif /* GROB_INTERFACE_HH */
