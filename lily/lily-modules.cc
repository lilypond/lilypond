/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2015 by David Kastrup <dak@gnu.org>

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

#include "lily-modules.hh"
#include "lily-imports.hh"

struct Scm_module::Variable_record
{
  const char *name_;
  Scm_variable *var_;
  Variable_record *next_;
  Variable_record (const char *name, Scm_variable *var, Variable_record *next_)
    : name_ (name), var_ (var), next_ (next_)
  { }
};

void
Scm_module::register_variable (const char *name, Scm_variable *var)
{
  variables_ = new Variable_record (name, var, variables_);
}

Scm_module::Scm_module (const char *name)
  : name_ (name), module_ (SCM_UNDEFINED), variables_ (0)
{
}

void
Scm_module::boot_init (void *arg)
{
  Scm_module *self = static_cast<Scm_module *> (arg);

  // Establish variables first
  for (Variable_record *p = self->variables_; p;)
    {
      Variable_record *next = p->next_;
      p->var_->boot (p->name_);
      delete p;
      p = next;
    }
  self->variables_ = 0;
}

void
Scm_module::boot ()
{
  assert (SCM_UNBNDP (module_));
  module_ = scm_c_define_module (name_, boot_init, static_cast <void *> (this));
}

void
Scm_module::import ()
{
  assert (SCM_UNBNDP (module_));
  SCM interface = scm_c_resolve_module (name_);
  // Using only the public interface is a voluntary form of access
  // control in GUILE.  It would be cumbersome to do so until
  // Guile_user itself is imported.
  if (SCM_MODULEP (Guile_user::module.module_))
    interface = Guile_user::module_public_interface (interface);
  for (Variable_record *p = variables_; p;)
    {
      Variable_record *next = p->next_;
      p->var_->import (interface, p->name_);
      delete p;
      p = next;
    }
  variables_ = 0;
  module_ = interface;
}

void
Scm_variable::boot (const char *name)
{
  assert (!SCM_VARIABLEP (var_));
  var_ = scm_c_define (name, var_);
}

void
Scm_variable::import (SCM module, const char *name)
{
  assert (SCM_UNBNDP (var_));
  var_ = scm_c_module_lookup (module, name);
}


Scm_variable::Scm_variable (Scm_module &m, const char *name, SCM value)
  : var_ (value)
{
  assert (SCM_IMP (value));
  m.register_variable (name, this);
}
