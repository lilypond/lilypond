#if 0
/*   
  new-chord-tremolo-engraver.cc --  implement New_chord_tremolo_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */



class New_chord_tremolo_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  New_chord_tremolo_engraver();
protected:

  virtual bool do_try_music (Music *);
  virtual void do_process_music ();
};

New_chord_tremolo_engraver::New_chord_tremolo_engraver()
{
}

bool
New_chord_tremolo_engraver::do_try_music (Music * m)
{
  if (dynamic_cast<Repeated_music*> (m))
    {
      
    }
}

#endif
