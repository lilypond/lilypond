/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2010 Han-Wen Nienhuys <hanwen@lilypond.org>
  

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

#include  "lily-guile.hh"
#include  "main.hh"
#include  "warn.hh"

/*
  INIT
*/


typedef void (*Void_fptr) ();
vector<Void_fptr> *scm_init_funcs_;

void add_scm_init_func (void (*f) ())
{
  if (!scm_init_funcs_)
    scm_init_funcs_ = new vector<Void_fptr>;

  scm_init_funcs_->push_back (f);
}

void
ly_init_ly_module (void *)
{
  for (vsize i = scm_init_funcs_->size (); i--;)
    (scm_init_funcs_->at (i)) ();

  if (be_verbose_global)
    {
      progress_indication ("[");
      scm_display (scm_c_eval_string ("(%search-load-path \"lily.scm\")"),
		   scm_current_error_port ());
      progress_indication ("]\n");
    }

  scm_primitive_load_path (scm_from_locale_string ("lily.scm"));
}

SCM global_lily_module;

void
ly_c_init_guile ()
{
  global_lily_module = scm_c_define_module ("lily", ly_init_ly_module, 0);
  scm_c_use_module ("lily");
}
