/* 
  book-paper-def.cc --  implement Book_paper_def
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/

#include "ly-module.hh"
#include "paper-def.hh"
#include "dimensions.hh"
#include "book-paper-def.hh"
#include "ly-smobs.icc"
#include "font-metric.hh"
#include "virtual-font-metric.hh"
#include "scaled-font-metric.hh"

IMPLEMENT_SMOBS (Book_paper_def);
IMPLEMENT_DEFAULT_EQUAL_P (Book_paper_def);

Book_paper_def::Book_paper_def ()
{
  output_scale_ = 1.0;
  scaled_fonts_ = SCM_EOL;
  scope_ = SCM_EOL;
  smobify_self ();
  scaled_fonts_ = scm_c_make_hash_table (11);
  scope_ = ly_make_anonymous_module (false); 
}

Book_paper_def::Book_paper_def (Book_paper_def const & src)
{
  output_scale_ = src.output_scale_;
  scope_ = SCM_EOL;
  scaled_fonts_ = SCM_EOL;
  smobify_self ();
  scope_= ly_make_anonymous_module (false);
  if (is_module (src.scope_))
    ly_import_module (scope_, src.scope_);

  scaled_fonts_ = scm_c_make_hash_table (11); // copying is not done with live defs. hopefully.
}

Book_paper_def::~Book_paper_def ()
{
}

SCM
Book_paper_def::mark_smob (SCM m)
{
  Book_paper_def *mo = (Book_paper_def*) SCM_CELL_WORD_1 (m);

  scm_gc_mark (mo->scope_);
  return mo->scaled_fonts_;
}

int
Book_paper_def::print_smob (SCM s, SCM p, scm_print_state*)
{
  (void) s;
  scm_puts ("#<Book_paper>", p);
  return 1;
}

Font_metric*
Book_paper_def::find_scaled_font (Font_metric *f, Real m, SCM input_enc_name)
{
  Real lookup_mag = m;
  if (!dynamic_cast<Virtual_font_metric*> (f))
    lookup_mag /= output_scale_;
  
  SCM sizes = scm_hashq_ref (scaled_fonts_, f->self_scm (), SCM_BOOL_F);
  if (sizes != SCM_BOOL_F)
    {
      SCM met = scm_assoc (scm_make_real (lookup_mag), sizes);
      if (ly_c_pair_p (met))
	return unsmob_metrics (ly_cdr (met));
    }
  else
    sizes = SCM_EOL;
  
  /* Hmm. We're chaining font - metrics.  Should consider whether to
     merge virtual-font and scaled_font.  */
  SCM val = SCM_EOL;
  if (Virtual_font_metric * vf = dynamic_cast<Virtual_font_metric*> (f))
    {
      /*
	For fontify_atom (), the magnification and name must be known
	at the same time. That's impossible for

	  Scaled (Virtual_font (Font1,Font2))

	so we replace by

	  Virtual_font (Scaled (Font1), Scaled (Font2))

      */
      
      SCM lst = SCM_EOL;
      SCM *t = &lst;
      for (SCM s = vf->get_font_list (); ly_c_pair_p (s); s = ly_cdr (s))
	{
	  Font_metric *scaled = find_scaled_font (unsmob_metrics (ly_car (s)),
						  m, input_enc_name);
	  *t = scm_cons (scaled->self_scm (), SCM_EOL);
	  t = SCM_CDRLOC (*t);
	}

      vf = new Virtual_font_metric (lst);
      val = vf->self_scm ();
    }
  else
    {
      if (!ly_c_symbol_p (input_enc_name))
	{
#if 0
	  /* FIXME.*/
	  SCM var = ly_module_lookup (scope_, ly_symbol2scm ("inputencoding"));
	  input_enc_name = scm_variable_ref (var);
      
#endif
	  input_enc_name = ly_symbol2scm ("latin1"); 
	}

      val = Modified_font_metric::make_scaled_font_metric (input_enc_name,
							   f, lookup_mag);
    }

  sizes = scm_acons (scm_make_real (lookup_mag), val, sizes);
  scm_gc_unprotect_object (val);
  scm_hashq_set_x (scaled_fonts_, f->self_scm (), sizes);
  return unsmob_metrics (val);
}

Paper_def * 
Book_paper_def::scale_paper (Paper_def *pd) const
{
  SCM proc = ly_scheme_function ("scale-paper");
  SCM new_pap = scm_call_2 (proc, pd->self_scm (), self_scm ());

  scm_gc_protect_object (new_pap);

  Paper_def *p = unsmob_paper (new_pap);
  
  p->bookpaper_ = (Book_paper_def*) this;
  return p;
}

LY_DEFINE (ly_make_bookpaper, "ly:make-bookpaper",
	   1, 0, 0,
	   (SCM size),
	   "Make a paperbook, for staff space SIZE, which is in INTERNAL_UNIT.") 
{
  Book_paper_def *bp = new Book_paper_def ;

  SCM_ASSERT_TYPE (ly_c_number_p (size), size,
		   SCM_ARG1, __FUNCTION__, "number");
  
  bp->output_scale_ = (ly_scm2double (size)) MM;

  return scm_gc_unprotect_object (bp->self_scm ());
}

LY_DEFINE (ly_bookpaper_fonts, "ly:bookpaper-fonts",
	   1, 0, 0,
	   (SCM bp),
	   "Return fonts scaled up BP")
{
  Book_paper_def *b = unsmob_book_paper_def (bp);
  
  SCM_ASSERT_TYPE (b, bp, SCM_ARG1, __FUNCTION__, "bookpaper");

  SCM func = ly_scheme_function ("hash-table->alist");

  SCM ell = SCM_EOL;
  for (SCM s = scm_call_1 (func, b->scaled_fonts_); ly_c_pair_p (s);
       s = ly_cdr (s))
    {
      SCM entry = ly_car (s);
      for (SCM t = ly_cdr (entry); ly_c_pair_p (t); t  = ly_cdr (t))
	{
	  Font_metric *fm = unsmob_metrics (ly_cdar (t));

	  if (dynamic_cast<Modified_font_metric*> (fm))
	    ell = scm_cons (fm->self_scm (), ell);
	}
    }
  return ell;
}


LY_DEFINE (ly_bookpaper_outputscale, "ly:bookpaper-outputscale",
	  1, 0, 0,
	  (SCM bp),
	  "Get outputscale for BP.")
{
  Book_paper_def *b = unsmob_book_paper_def (bp);
  SCM_ASSERT_TYPE (b, bp, SCM_ARG1, __FUNCTION__, "bookpaper");
  return scm_make_real (b->output_scale_);
}


SCM
Book_paper_def::lookup_variable (SCM sym) const
{
  SCM var = ly_module_lookup (scope_, sym);

  return scm_variable_ref (var);
}

SCM
Book_paper_def::c_variable (String s) const
{
  return lookup_variable (ly_symbol2scm (s.to_str0 ()));
}
