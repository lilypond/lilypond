/* 
  guile-init.cc -- implement GUILE init routines.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006--2009 Han-Wen Nienhuys <hanwen@lilypond.org>
  
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
