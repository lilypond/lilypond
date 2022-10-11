/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef INTERFACE_HH
#define INTERFACE_HH

#include "lily-guile.hh"
#include "protected-scm.hh"

class Grob;

#define ADD_INTERFACE(cl, b, c)                                                \
  Grob_interface<cl> cl##_interface_initializer;                               \
  template <>                                                                  \
  char const *Grob_interface<cl>::cxx_name_ (#cl);                             \
  template <>                                                                  \
  char const *Grob_interface<cl>::description_ (b);                            \
  template <>                                                                  \
  char const *Grob_interface<cl>::variables_ (c);

SCM add_interface (char const *cxx_name, char const *descr, char const *vars);

SCM ly_add_interface (SCM, SCM, SCM);
void internal_add_interface (SCM, SCM, SCM);
SCM ly_all_grob_interfaces ();

template <class Interface>
class Grob_interface
{
public:
  Grob_interface () { add_scm_init_func (Grob_interface::init); }

private:
  static void init ()
  {
    interface_symbol_ = ::add_interface (cxx_name_, description_, variables_);
  }

  template <class T>
  friend bool has_interface (Grob const *);

private:
  static Protected_scm interface_symbol_;
  static char const *cxx_name_;
  static char const *description_;
  static char const *variables_;
};

template <class Interface>
Protected_scm Grob_interface<Interface>::interface_symbol_;

#endif /* INTERFACE_HH */
