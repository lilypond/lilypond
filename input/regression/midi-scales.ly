
\version "2.19.21"
% candidate for regression.  -gp
\header {
  texidoc="Converting LilyPond input to MIDI and then again back with
  @code{midi2ly.py} is a reversible procedure in some simple cases,
  which mean that the original @code{.ly} -file and the one converted 
  back from the generated @code{.midi} -file do not differ.
  Here are produced some scales.
  
  "
}

%{
  This means, doing:

    lilypond input/test/midi-scales.ly
    midi2ly midi-scales.midi
    diff -u input/test/midi-scales.ly midi-scales-midi.ly

  should show no differences at all in \key commands or notes.

  Therefore, do not reformat this file unless midi2ly changes.

  1.7.30 reformatted, because
  midi2ly now outpts 1 bar per line and adds bar checks and numbers.

%}

scales =  \relative {

  % [INSTRUMENT_NAME] bright acoustic
  \key c \major  % sharp-major
  c'4 d e f |
  g a b c |

  \key g \major
  g a b c |
  d e fis g |

  \key d \major
  d, e fis g |
  a b cis d |

  \key a \major
  a b cis d |
  e fis gis a |

  \key e \major
  e, fis gis a |
  b cis dis e |

  \key b \major
  b cis dis e |
  fis gis ais b |

  \key fis \major
  fis, gis ais b |
  cis dis eis fis |

  \key cis \major
  cis, dis eis fis |
  gis ais bis cis |

  \key a \minor  % sharp-minor
  a b c d |
  e f gis a |

  \key e \minor
  e, fis g a |
  b c dis e |

  \key b \minor
  b cis d e |
  fis g ais b |

  \key fis \minor
  fis, gis a b |
  cis d eis fis |

  \key cis \minor
  cis, dis e fis |
  gis a bis cis |

  \key gis \minor
  gis ais b cis |
  dis e fisis gis |

  \key dis \minor
  dis, eis fis gis |
  ais b cisis dis |

  \key ais \minor
  ais bis cis dis |
  eis fis gisis ais |

  \key f \major  % flat-major
  f, g a bes |
  c d e f |

  \key bes \major
  bes c d ees |
  f g a bes |

  \key ees \major
  ees,, f g aes |
  bes c d ees |

  \key aes \major
  aes, bes c des |
  ees f g aes |

  \key des \major
  des,, ees f ges |
  aes bes c des |

  \key ges \major
  ges, aes bes ces |
  des ees f ges |

  \key ces \major
  ces,, des ees fes |
  ges aes bes ces |

  \key d \minor  % flat-minor
  d, e f g |
  a bes cis d |

  \key g \minor
  g, a bes c |
  d ees fis g |

  \key c \minor
  c,, d ees f |
  g aes b c |

  \key f \minor
  f, g aes bes |
  c des e f |

  \key bes \minor
  bes,, c des ees |
  f ges a bes |

  \key ees \minor
  ees, f ges aes |
  bes ces d ees |

  \key aes \minor
  aes, bes ces des |
  ees fes g aes |
}

\score {
  \context Voice \scales
  \layout { }
  \midi { }
}

