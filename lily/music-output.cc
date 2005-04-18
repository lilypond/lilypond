/* 
  music-output.cc --  implement Music_output
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/

#include "music-output.hh" 
#include "ly-smobs.icc"

Music_output::Music_output ()
{
  smobify_self ();
}

void
Music_output::process ()
{
}

Music_output::~Music_output ()
{
  
}
  
void
Music_output::derived_mark () const
{
}

IMPLEMENT_SMOBS (Music_output);
IMPLEMENT_DEFAULT_EQUAL_P (Music_output);
IMPLEMENT_TYPE_P (Music_output, "ly:music-output?");

SCM
Music_output::mark_smob (SCM s)
{
  Music_output *sc = (Music_output *) SCM_CELL_WORD_1 (s);

  sc->derived_mark ();
  return SCM_EOL;
}

int
Music_output::print_smob (SCM, SCM p, scm_print_state*)
{
  scm_puts ("#<Music_output>", p);

  return 1;
}
