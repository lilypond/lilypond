#include "book.hh"
#include "output-def.hh"
#include "score.hh"
#include "ly-module.hh"

LY_DEFINE(ly_make_book, "ly:make-book",
	  2, 0, 1, (SCM paper, SCM header, SCM scores),
	  "Make a \\book of @var{paper} and @var{header} (which may be #f as well)  "
	  "containing @code{\\scores}.")
{
  Output_def * odef = unsmob_output_def (paper);
  SCM_ASSERT_TYPE (odef, paper,
		   SCM_ARG1, __FUNCTION__, "Output_def");

  Book *book = new Book;
  book->paper_ = odef;

  if (ly_c_module_p (header))
    book->header_ = header;
  
  for (SCM s = scores; scm_is_pair (s); s = scm_cdr (s))
    {
      Score *score = unsmob_score (scm_car (s));
      if (score)
	book->scores_.push (score);
    }
  
  SCM x = book->self_scm ();
  scm_gc_unprotect_object (x);
  return x;
}
