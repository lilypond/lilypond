% Ugh, we need to override some LaTeX titling stuff
\header {
  title =	"Ouvertüre\\vrule width0pt height 0pt depth 1ex";
  subtitle =	"\\normalsize Zu Heinrich Joseph v. Collins Trauerspiel\\vrule width0pt height 0pt depth 1ex";
  subsubtitle =	"\\Large\\bf Coriolan";
  opus =  "Op. 62";
  piece = "\\hspace*{30mm}\\normalfont\\large Allegro con brio";
  composer = 	 "Ludwig van Beethoven (1770-1827)";
  enteredby = 	 "JCN";
  instrument = \instrument;

  % mutopia headers.
  mutopiatitle = "Ouverture Coriolan";
  mutopiacomposer = "L.v.Beethoven (1770-1827)";
  mutopiaopus = "O62";
  mutopiainstrument = \instrument;
  date = "1807";

  source = "Six Great Overtuers in Full Score, Ludwig van Beethoven; Dover
   Publications INC, New York (1985).  Unabridged republication of
   Beethoven's Werke, Ouverturen fuer Orchester:  No. 18-21, 26-27;
   Breitkopf and Haertel, Leipzig, n.d. [1862-1865].";

  %{

  i.  Six Great Overtuers in Full Score, Ludwig van Beethoven; Dover
      Publications INC, New York (1985).  Free of copyright and
      copyright marks.  Reprint.  Originally published: Breitkopf and
      Haertel, n.d. [1862-1865] (Ludwig van Beethoven's Werke; Ser 3)
      No. 18-21, 26-27.
      M1004.B4097
      ISBN 0-486-24789-9

  Differences of Mutopia score

  The printed output of this Mutopia score exhibits a number of
  differences to the original source i.  These differences fall into
  two catogaries: changes made by the editor (enteredby), and
  automatic changes made by the GNU LilyPond typesetting mechanism.

    a. Changes by the editor

      1. The old notation of dotted quarter notes on the fourth beat
         are modernised to a quarter note, a tie, and an eight note,
         ie:

	     \partial 4; c4.  ->  c4 ~ | c8

	 GNU LilyPond 1.3.136 can not handle the old notation.  When
	 this old notation will be supported, it should be fairly
	 straightforward to find and change these instances.  It would
	 be advisable, however, to keep the current, modern printout,
	 and document this as a change in b.
	 
      2. Notes are entered by their name, no efforts were made to
         force the printing of reminder accidentals to mimic the
         original edition, see b.2,3.
      
      3. Empty staffs are removed through the Hara-kiri-staff
         mechanism.  This change can be undone very easily.

      4. Margins are changed to Mutopia dictated values.  This change
         can be undone very easily, however, see b.7.
	     
    b. Automatic changes by the GNU LilyPond typesetting mechanism

      1. Bar lines are not connected between staff groups.

      2. Accidentals are not repeated within one bar, in

           \key es\major; b2 b

	 only the first b gets an accidental.  Note that not in all
	 cases these accidentals are consistently repeated, however,
	 they are printed in most cases where the two notes are
	 more than one note apart.

      3. Reminder accidentals are not printed, in

           \key es\major; b1 | bes

	 no flat is printed with the bes.

      4. Stem.default-neutral-direction and
         Beam.default-neutral-direction are 1 (up).

      5. Beam-dir-algorithm is 'majority.

      6. The part combiner prints "I.", "II." and "`a 2." strings
         where appropriate.

      7. Line breaks are calculated.

      8. GNU LilyPond has bugs that affect the Coriolan, see test
         sources in input/bugs/*.ly.

      
  Other editions.
    
  ii.  Edition Eulenburg No.626 Coriolan, Overture for Orchestra
       Op. 62; Ernst Eulenburg LTD, (not dated).  Edited 1936 by
       dr. Max Unger from and following score autograph and original
       published parts, ie, Stimmen-Drucke N.Simrock Bonn (most
       probably from 1807) and Industriekontor Vienna (1807).
  
  iii.  Beethoven's Werke -- Ouverturen fuer Orchester; Breitkopf
        u. Haertel, Leipzig (not dated, but very old and worn).
  
  iv.  Beethoven Werke II,1 -- Ouverturen und Wellingtons Sieg;
       G. Henle Verlag, Muenchen (1974).  Veroeffentlichung des
       Beethovenhauses in Bonn.  Die ausgabe wurde durch die
       Unterstuetzung des Landes Nordrhein-Westfalen ermoeglicht.


  Background.
  
  From Overture Coriolan is no Urtext edition available, although
  there are Urtext editions of Beethoven's symphonies.
  
  Edition Eulenburg is the ``first after many years'' that tries to
  stay as close to the original text as possible.  During the 1900s,
  it was customary for editors not to respect the original text and
  make any changes they thought necessary.  Unger made corrections for
  a few ``small and obvious'' inaccuracies.  Together with Breitkopf
  and Haertel, this is the only score edition to abbreviate (LilyPond
  source notation)

      [es16 es g g] [b b c c]
  
  using the notation
  
      [es8:16 g: b: c:]

  editions of individual parts by Breitkopf and Haertel use this
  common practice abbreviation too.
  
  Edition Beethoven's Werke by Breitkopf and Haertel comes without any
  commentary, copyright mark or date.  There are no significant
  differences with ed. Unger.  The copy was very old and worn, but is
  probably younger than ed. Unger.

  Edition Beethoven Werke by G. Henle was a new study based upon the
  three main sources, the score autograph, the published parts
  Stimmen-Drucke N.Simrock Bonn (most probably from 1807) and
  Industriekontor Vienna (1807).  They reinforce the assumptions made
  by Unger that both published parts were first prints, noting that
  both must have worked from a different copy of the autograph (and
  not from the autograph itself), and that Simrock's is a bit closer
  to the original autograph.  This edition is supposed to deviate from
  the autograph only in using `modernised notation' for the following
  cases (GNU LilyPond source notation), as described by the foreword.

  a. Use dots instead of ties for sustaining of notes when there's no
     measure break:
  
       c4 ~ c8  ->  c4.

     We very much doubt this, as the 1862 edition already has this,
     but in contrast, has a different change, see i.a.1 above.

  b. Don't repeat accidentals within the same measure:
  
       \key es \major; b4 c b! c |   ->   \key es \major; b4 c b c

     We think that also other changes may have been made, see i.b.3
     above.
  
  c. Start slurring from first tied note, not from second:

       c4 ~ c8 ( d ) e   ->   c4 ( ~ c8 d ) e

  From these deviations, the third is the only one unique to this
  edition, but these are easy to spot and mostly parenthesed; the
  other two deviations are standard modern day music notation practice
  and thus common to all editions.

  %}
    
  style = "Classical";
  copyright = "Public Domain";
  maintainer = "Jan Nieuwenhuizen";
  maintainer_email = "janneke@gnu.org";
  lastupdated =	 "2001/Mar/19";
  mutopiapublicdomain = "\\parbox{\hsize}{\\thefooter\\quad\\small
    \\\\This music is part of the Mutopia project,
    \\texttt{http://www.mutopiaproject.org/}\\\\It has been typeset
    and placed in the public domain by " + \maintainer +
    ".\\\\Unrestricted modification and redistribution is permitted
    and encouraged---copy this music and share it.}";
 tagline = \mutopiapublicdomain;
 footer = "Mutopia-2001/03/19-57";
}
