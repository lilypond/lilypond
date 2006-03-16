%{
Welcome to LilyPond
===================

Congratulations, LilyPond has been installed successfully.

Now to take it for the first test run.

  1. Save this LilyPond file on your desktop.

  2. Pick it up from the desktop with your mouse pointer, drag and drop
     it onto the LilyPond icon.

  3. LilyPond automatically produces a PDF file from the musical scale
     below.

  4. To print or view the result, click on the newly produced file called

        Welcome_to_LilyPond.PDF


That's it.  For more information, visit http://lilypond.org .
%}

\header{
  title = "A scale in LilyPond"
}

\relative{
  c d e f g a b c
}


\version "2.7.36"  % necessary for upgrading to future LilyPond versions.
