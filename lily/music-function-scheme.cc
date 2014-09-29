#include "music-function.hh"

LY_DEFINE (ly_music_function_extract, "ly:music-function-extract", 1, 0, 0,
           (SCM x),
           "Return the Scheme function inside@tie{}@var{x}.")
{
  LY_ASSERT_SMOB (Music_function, x, 1);

  return Music_function::unsmob (x)->get_function ();
}

LY_DEFINE (ly_music_function_signature, "ly:music-function-signature", 1, 0, 0,
           (SCM x),
           "Return the function signature inside@tie{}@var{x}.")
{
  LY_ASSERT_SMOB (Music_function, x, 1);

  return Music_function::unsmob (x)->get_signature ();
}

LY_DEFINE (ly_make_music_function, "ly:make-music-function", 2, 0, 0,
           (SCM signature, SCM func),
           "Make a function to process music, to be used for the"
           " parser.  @var{func} is the function, and @var{signature}"
           " describes its arguments.  @var{signature}'s cdr is a list"
           " containing either @code{ly:music?} predicates or other type"
           " predicates.  Its car is the syntax function to call.")
{
  LY_ASSERT_TYPE (ly_is_list, signature, 1);
  LY_ASSERT_TYPE (ly_is_procedure, func, 2);
  int n = 0;
  for (SCM p = signature; scm_is_pair (p); p = scm_cdr (p), ++n)
    {
      SCM proc = scm_car (p);
      if (scm_is_pair (proc))
        proc = scm_car (proc);
      if (scm_is_false (scm_procedure_p (proc)))
        {
          scm_wrong_type_arg_msg ("music-function", n, p,
                                  "music function predicate");
        }
    }

  return Music_function::make_smob (signature, func);
}
