/*   
  context-specced-music.hh -- declare Context_specced_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CONTEXT_SPECCED_MUSIC_HH
#define CONTEXT_SPECCED_MUSIC_HH

#include "music-wrapper.hh"

class Context_specced_music : public Music_wrapper
{
public:
  /// The kind of translation needed for this music.  
  String translator_type_str_;

  /// what identification for the translation unit
  String translator_id_str_;
  Context_specced_music (Music*);
  VIRTUAL_COPY_CONS(Music);

};
#endif /* CONTEXT_SPECCED_MUSIC_HH */

