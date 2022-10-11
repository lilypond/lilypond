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

#include "lily-modules.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "warn.hh"

class Scm_module::Variable_record
{
public:
  const char *name_;
  Scm_variable *var_;
  Variable_record *next_;
  Variable_record (const char *name, Scm_variable *var, Variable_record *next_)
    : name_ (name),
      var_ (var),
      next_ (next_)
  {
  }
};

void
Scm_module::register_variable (const char *name, Scm_variable *var)
{
  variables_ = new Variable_record (name, var, variables_);
}

Scm_module::Scm_module (const char *name)
  : name_ (name),
    module_ (SCM_UNDEFINED),
    variables_ (0)
{
}

void
Scm_module::boot_init (void *arg)
{
  Scm_module *self = static_cast<Scm_module *> (arg);

  // Establish variables
  for (Variable_record *p = self->variables_; p; p = p->next_)
    p->var_->boot (p->name_);
}

static SCM
call_trampoline (void *self)
{
  // One more indirection since void * can only be safely cast to
  // pointers to data rather than pointers to function.
  (*static_cast<void (**) ()> (self)) ();
  return SCM_UNSPECIFIED;
}

void
Scm_module::boot (void (*init) ())
{
  assert (SCM_UNBNDP (module_));
  module_ = scm_c_define_module (name_, boot_init, static_cast<void *> (this));
  // Can't wrap the following in the scm_c_define_module call since
  // the init code may need module_ operative.
  if (init)
    scm_c_call_with_current_module (module_, call_trampoline,
                                    static_cast<void *> (&init));
  // Verify that every Variable has a definition, either because of
  // getting initialized with a value at definition or because of the
  // init call providing one.
  for (Variable_record *p = variables_; p;)
    {
      Variable_record *next = p->next_;
      if (SCM_UNBNDP (SCM (*p->var_)))
        error (
          _f ("Uninitialized variable `%s' in module (%s)", p->name_, name_));
      delete p;
      p = next;
    }
  variables_ = 0;
}

void
Scm_module::import ()
{
  assert (SCM_UNBNDP (module_));
  SCM intrface = scm_c_resolve_module (name_);
  // Using only the public interface is a voluntary form of access
  // control in GUILE.  It would be cumbersome to do so until
  // Guile_user itself is imported.
  if (SCM_MODULEP (Guile_user::module.module_))
    intrface = Guile_user::module_public_interface (intrface);
  for (Variable_record *p = variables_; p;)
    {
      Variable_record *next = p->next_;
      p->var_->import (intrface, p->name_);
      delete p;
      p = next;
    }
  variables_ = 0;
  module_ = intrface;
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
