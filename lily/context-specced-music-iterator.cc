/*   
context-specced-music-iterator.cc --  implement 

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "music-wrapper-iterator.hh"
#include "translator-group.hh"

class Context_specced_music_iterator : public Music_wrapper_iterator
{
public:
  VIRTUAL_COPY_CONS(Music_iterator);
  DECLARE_SCHEME_CALLBACK(constructor,());
  virtual void construct_children ();
};

void
Context_specced_music_iterator::construct_children ()
{
  SCM ct = get_music ()->get_mus_property ("context-type");
  String c_type;
  if (gh_string_p (ct))
    c_type =  ly_scm2string (ct);
  
  String c_id;
  SCM ci = get_music ()->get_mus_property ("context-id");
  if (gh_string_p (ci))
    c_id = ly_scm2string (ci);
  
  Translator_group* a
    =report_to ()->find_create_translator (c_type, c_id);
  
  set_translator (a);

  Music_wrapper_iterator::construct_children();
}
IMPLEMENT_CTOR_CALLBACK(Context_specced_music_iterator);
