/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2023 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "cpu-timer.hh"
#include "international.hh"
#include "lily-guile.hh"
#include "lily-imports.hh"
#include "ly-scm-list.hh"
#include "main.hh"
#include "smobs.hh"
#include "time-tracer.hh"
#include "warn.hh"

#include <string_view>

using namespace std::literals;

/*
  INIT
*/

// Why a pointer here?  Because it has zero initialization at load
// time which is guaranteed to come before the static initializations
// of all constructors for static expressions of the classes created
// by ADD_SCM_INIT_FUNC.  The vector data type does not have load-time
// initialization and might clear out already set callbacks at the
// time it is initialized since there is no implied order among
// non-trivial constructors for static data in separate compilation
// units.  So we need a trivial type like a pointer instead.

typedef void (*Void_fptr) ();
std::vector<Void_fptr> *scm_init_funcs_;

void
add_scm_init_func (void (*f) ())
{
  if (!scm_init_funcs_)
    scm_init_funcs_ = new std::vector<Void_fptr>;

  scm_init_funcs_->push_back (f);
}

void
ly_init_ly_module ()
{
  // Start up type system first.
  {
    auto trace_slice = tracer_global.log_scope ("Scm_init::init"sv);
    Scm_init::init ();
  }

  {
    auto trace_slice = tracer_global.log_scope ("scm_init_funcs"sv);
    for (Void_fptr f : *scm_init_funcs_)
      f ();
  }

  /*
     Guile tries to optimize code when byte-compiling.  Experimentally, this
     makes LilyPond's speed borderline worse, not better, and the compilation
     takes time.  Thus, we turn off all optimizations.  This is also used when
     compiling Scheme code in user .ly files. */
  Compile::default_optimization_level (to_scm (0));

  Cpu_timer timer;
  if (is_loglevel (LOG_DEBUG))
    {
      debug_output ("[", true);
      // FIXME: scm_primitive_load_path may load a compiled version of the code;
      //        can this be detected and printed? If not, remove this output.
      scm_display (scm_c_eval_string ("(%search-load-path \"lily/lily.scm\")"),
                   scm_current_error_port ());
      debug_output ("]\n", false);
    }

  {
    auto trace_slice = tracer_global.log_scope ("primitive-load-path lily"sv);
    scm_primitive_load_path (scm_from_latin1_string ("lily/lily"));
    debug_output (
      _f ("(primitive-load-path lily): %.2f seconds", timer.read ()));
  }
}

void
ly_c_init_guile ()
{
  Guile_user::module.import ();
  Compile::module.import ();
  Lily::module.boot (ly_init_ly_module);
  Loader::module.import ();
  Page::module.import ();
  Srfi_1::module.import ();
  Syntax::module.import ();
  Display::module.import ();
  Goops::module.import ();
  Hash_table::module.import ();
  Unicode::module.import ();

  {
    auto trace_slice = tracer_global.log_scope ("scm_c_use_module lily"sv);
    scm_c_use_module ("lily");
  }
}
