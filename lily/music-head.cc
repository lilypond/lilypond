/* 
  music-head.cc --  implement Music_head
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/
#include "music-head.hh"
#include "string.hh"

/*
  C&P from example/box.c
 */

static scm_t_bits music_head_tag;

/* Print a textual represenation of the smob to a given port.  */
static int
print_music_head (SCM b, SCM port, scm_print_state *)
{
  SCM value = SCM_CELL_OBJECT_1 (b);

  scm_puts ("#<packaged object ", port);
  scm_write (value, port);
  scm_puts (">", port);

  /* Non-zero means success.  */
  return 1;
}


/* This defines the primitve `make-music_head', which returns a new smob of
   type `music_head', initialized to `#f'.  */
LY_DEFINE (ly_make_music_head, "ly:make-music-head", 2, 0, 0,
	   (SCM func, SCM signature),
	  "Make a function to process music, to be used for the "
	   "parser. @code{func} is the function, and @code{signature} describes "
	   "Its arguments. @code{signature} is a list containing either "
	   "@code{ly:music?} predicates or other type predicates.")
{
  /* This macro creates the new objects, stores the value `#f' into it
     and returns it to the caller.  */
  String str = "";

  int k = 0;
  for (SCM s = signature; ly_c_pair_p (s); s = ly_cdr (s))
    {
      if (str != "")
	str += "-";
      
      if (ly_car (s) == Music_type_p_proc)
	str += "music";
      else if (ly_procedure_p (ly_car (s)))
	str += "scm";

      k++;
    }
  
  scm_set_object_property_x (func, ly_symbol2scm ("music-head-signature"),
			     signature);
  
  scm_set_object_property_x (func, ly_symbol2scm ("music-head-signature-keyword"),
			     ly_symbol2scm (str.to_str0 ()));
    
  SCM_RETURN_NEWSMOB (music_head_tag, func);
}

bool
is_music_head (SCM music_head)
{
  return (SCM_NIMP (music_head) && SCM_CELL_TYPE (music_head) == music_head_tag);
}


/* This is the primitive `music_head-ref' which returns the object stored in
   the music_head.  */
SCM
get_music_head_transform (SCM music_head)
{
  if (!is_music_head (music_head))
    return SCM_UNDEFINED;
  
  return SCM_CELL_OBJECT_1 (music_head);
}

static void
init_music_head (void)
{
  music_head_tag = scm_make_smob_type ("music-head", 0);
  scm_set_smob_mark (music_head_tag, scm_markcdr);
  scm_set_smob_print (music_head_tag, print_music_head);
}
ADD_SCM_INIT_FUNC (music_head_tag, init_music_head); 
