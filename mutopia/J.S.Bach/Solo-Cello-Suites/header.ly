%{
Header for Solo Cello Suite no. 2.

This is the 2nd cello suite by Bach, in versions for both cello and
viola.  It was coded by JCN following a Baerenreiter
urtext. Optionally you can add or remove slurs etc. to taste. 

The setup of the files is slightly complicated, because sharing of
information is taken to the extreme.

%}

\header{
  title =	 "Solo Cello Suites";
  subtitle =	 "Suite II";
  opus =	 "BWV 1008";
  composer =	 "Johann Sebastian Bach (1685-1750)";
  enteredby =	 "JCN";
  instrument =	 \instrument;

  % mutopia headers.
  mutopiatitle = "Solo Cello Suites, Suite II";
  mutopiacomposer = "J.S.Bach (1685-1750)";
  mutopiaopus =	"BWV1008";
  mutopiainstrument = \intrument;
  date = "1710s, 1720s";
  source = "Baerenreiter urtext";
  style = "Baroque";
  copyright = "Public Domain";
  maintainer = "Jan Nieuwenhuizen";
  maintainer_email = "janneke@gnu.org";
  lastupdated =	 "2001/Jan/31";
  mutopiapublicdomain = "\\parbox{\hsize}{\\thefooter\\quad\\small
    \\\\This music is part of the Mutopia project,
    \\texttt{http://www.mutopiaproject.org/}\\\\It has been typeset
    and placed in the public domain by " + \maintainer +
    ".\\\\Unrestricted modification and redistribution is permitted
    and encouraged---copy this music and share it.}";
 tagline = \mutopiapublicdomain;
 footer = "Mutopia-2001/01/31-2";
}
