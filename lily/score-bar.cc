/*   
  score-bar.cc --  implement Score_bar
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "score-bar.hh"



void
Score_bar::do_pre_processing ()
{
  type_str_ = "|";
  if (break_status_dir() != RIGHT) 
    {
      set_empty (true);
      transparent_b_ = true;
    }
}
