% Ugh, we need to override some LaTeX titling stuff
\header {
  title =	"Ouverture\\vrule width0pt height 0pt depth 1ex";
  subtitle =	"\\normalsize Zu Heinrich Joseph v. Collins Trauerspiel\\vrule width0pt height 0pt depth 1ex";
  subsubtitle =	"\\Large\\bf Coriolan";
  opus =  "Op. 62";
  piece = "\\normalfont\\large Allegro con brio";
  composer = 	 "Ludwig van Beethoven (1770-1827)";
  enteredby = 	 "JCN";
  instrument = \instrument;

  % mutopia headers.
  mutopiatitle = "Ouverture Coriolan";
  mutopiacomposer = "L.v.Beethoven (1770-1827)";
  mutopiaopus = "O62";
  mutopiainstrument = \instrument;
  date = "1807";

  %{

  From Ouverture Coriolan is no Urtext edition available, although
  there are Urtext editions of Beethoven's symphonys.  The Eulenburg
  edition is commonly regarded as the Coriolan Urtext.
  
  Edition Eulenburg is the ``first after many years'' that tries to
  stay as close to the original text as possible.  During the 1900s,
  it was customary for editors not to respect the original text and
  make any changes they thought necessary.  Unger made corrections for
  a few ``small and obvious'' inaccuracies.  This is the only score
  edition to abbreviate (LilyPond source notation)

      [es16 es g g] [b b c c]
  
  using the notation
  
      [es8:16 g: b: c:]

  however, editions of individual parts by Breitkopf and Haertel use
  this common practice abbreviation too.
  
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
  cases (LilyPond source notation).

  i. Use dots instead of ties for sustaining of notes when there's no
  measure break:
  
      c4 ~ c8  ->  c4.

  ii. Don't repeat accidentals within the same measure:
  
      \key es \major; b4 c b! c |   ->   \key es \major; b4 c b c

  iii. Start slurring from first tied note, not from second:

      c4 ~ c8 ( d ) e   ->   c4 ( ~ c8 d ) e

  From these deviations, the third is the only one unique to this
  edition, but these are easy to spot and mostly parenthesed; the
  other two deviations are standard modern day music notation practice
  and thus common to all editions.

  %}
    
  source = "i. Ed. Eulenburg, edited by dr. Max Unger 1936 from and
  following score autograph and original published parts, ie,
  Stimmen-Drucke N.Simrock Bonn (most probably from 1807) and
  Industriekontor Vienna (1807).\\\\
  ii. Beethoven's Werke -- Ouverturen fuer Orchester; Breitkopf u. Haertel,
  Leipzig (not dated, but very old and worn).\\\\
  iii.  Beethoven Werke II,1 -- Ouverturen und Wellingtons Sieg; G. Henle
  Verlag, Muenchen (1974).
  Veroeffentlichung des Beethovenhauses in Bonn.
  Die ausgabe wurde durch die Unterstuetzung des Landes Nordrhein-Westfalen
  ermoeglicht.";
  
  style = "Classical";
  copyright = "Public Domain";
  maintainer = "Jan Nieuwenhuizen";
  maintainer_email = "janneke@gnu.org";
  lastupdated =	 "2001/Feb/27";
  mutopiapublicdomain = "\\parbox{\hsize}{\\thefooter\\quad\\small
    \\\\This music is part of the Mutopia project,
    \\texttt{http://www.mutopiaproject.org/}\\\\It has been typeset
    and placed in the public domain by " + \maintainer +
    ".\\\\Unrestricted modification and redistribution is permitted
    and encouraged---copy this music and share it!}";
 tagline = \mutopiapublicdomain;
 footer = "pre-Mutopia-200y/mm/dd-nr";
}
