/*
  Junk String numbers.
  
 */

#include "engraver.hh"

class String_number_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (String_number_engraver);
protected:
  virtual bool try_music (Music* m);
};


bool
String_number_engraver::try_music (Music * )
{
  return true;
}

String_number_engraver::String_number_engraver ()
{

}

/*
  TODO: string numbers are printed right of the note circled. This
  engraver should provide this functionality.
  
 */

ADD_TRANSLATOR (String_number_engraver,
/* descr */       "Swallow string-number-events - the purpose of this engraver is to"
" process tab for normal notation. To provent warnings for unprocessed "
" string-number-event to obscure real error messages, this engraver "
" swallows them all.",
		  
/* creats*/       "",
/* accepts */     "string-number-event",
/* acks  */      "",
/* reads */       "",
/* write */       "");
