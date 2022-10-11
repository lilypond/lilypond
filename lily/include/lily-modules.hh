/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2015--2022 by David Kastrup <dak@gnu.org>

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

#ifndef LILY_MODULES_HH
#define LILY_MODULES_HH

#include "lily-guile.hh"

#include <utility>

class Scm_variable;

class Scm_module
{
  const char *name_;
  SCM module_;
  class Variable_record;
  Variable_record *variables_;
  static void boot_init (void *);

public:
  void boot (void (*init) () = 0);
  void import ();
  void register_variable (const char *name, Scm_variable *var);

  Scm_module (const char *name);

  operator SCM ()
  {
    assert (SCM_MODULEP (module_));
    return module_;
  }
};

class Scm_variable
{
  SCM var_;
  friend Scm_module;
  void boot (const char *name);
  void import (SCM module, const char *name);

public:
  operator SCM & ()
  {
#if 0
    // One could conceivably work with variables even before the
    // module is initialized
    return SCM_VARIABLEP (var_) ? *SCM_VARIABLE_LOC (var_) : var_;
#endif
#if DEBUG
    assert (SCM_VARIABLEP (var_));
#endif
    return *SCM_VARIABLE_LOC (var_);
  }
  void operator= (SCM const &k)
  {
    *SCM_VARIABLE_LOC (var_) = k;
  }
  template <typename... Args>
  SCM operator() (Args &&...args)
  {
    return ly_call (*this, std::forward<Args> (args)...);
  }
  Scm_variable (Scm_module &m, const char *name, SCM value = SCM_UNDEFINED);
};

template <Scm_module &m>
class Module_variable : public Scm_variable
{
public:
  Module_variable (const char *name, SCM value = SCM_UNDEFINED)
    : Scm_variable (m, name, value)
  {
  }
  void operator= (SCM const &k) { Scm_variable::operator= (k); }
};

#endif
