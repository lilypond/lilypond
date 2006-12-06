#include "music-function.hh"

LY_DEFINE (ly_music_function_p, "ly:music-function?", 1, 0, 0,
	   (SCM x),
	   "Is @var{x} an @code{music-function}?")
{
  return is_music_function (x) ? SCM_BOOL_T : SCM_BOOL_F;
}
		 
LY_DEFINE (ly_music_function_extract, "ly:music-function-extract", 1, 0, 0,
	   (SCM x),
	   "Return the Scheme function inside @var{x}")
{
  SCM_ASSERT_TYPE(is_music_function (x), x, SCM_ARG1, __FUNCTION__,
		  "music function");
  
  return SCM_CELL_OBJECT_1(x);
}

LY_DEFINE (ly_make_music_function, "ly:make-music-function", 2, 0, 0,
	   (SCM signature, SCM func),
	   "Make a function to process music, to be used for the "
	   "parser. @code{func} is the function, and @code{signature} describes "
	   "Its arguments. @code{signature} is a list containing either "
	   "@code{ly:music?} predicates or other type predicates.")
{
  SCM_ASSERT_TYPE(ly_is_procedure (func), func, SCM_ARG1, __FUNCTION__, "function");
  return  make_music_function (signature, func);
}

