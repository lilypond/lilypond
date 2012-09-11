\version "2.16.0"

\header {
  tagline = ##f
  texidoc = "Text that can spread over pages is entered with the
@code{\\markuplist} command.  Widowed and orphaned lines are avoided
at the begininng and end of a @code{\\markuplist} containing more
than one line."
}

#(set-default-paper-size "a7")

#(define-markup-list-command (paragraph layout props args) (markup-list?)
  (interpret-markup-list layout props 
   (make-justified-lines-markup-list (cons (make-hspace-markup 2) args))))
\book {
  \markuplist {} % Empty list is handled gracefully
  %% Candide, Voltaire
  \markuplist \override-lines #'(baseline-skip . 3.0) {
    \paragraph { % The final two lines are placed on page 2.
      Il y avait en Westphalie, dans le château de M. le baron de
      Thunder-ten-tronckh, un jeune garçon à qui la nature avait donné
      les mœurs les plus douces.  Sa physionomie annonçait son âme.
      Il avait le jugement assez droit, avec l'esprit le plus simple ;
      c'est, je crois, pour cette raison qu'on le nommait Candide.  Les
      anciens domestiques de la maison soupçonnaient qu'il était fils
      de la sœur de monsieur le baron et d'un bon et honnête
      gentilhomme du voisinage, que cette demoiselle ne voulut jamais
      épouser parce qu'il n'avait pu prouver que soixante et onze
      quartiers, et que le reste de son arbre généalogique avait été
      perdu par l'injure du temps. (not orphaned)
    }
  }
  \markuplist \override-lines #'(baseline-skip . 3.9) {
    \paragraph {
      Monsieur le baron était un des plus puissants seigneurs de la
      Westphalie, car son château avait une porte et des fenêtres.  Sa
      grande salle même était ornée d'une tapisserie.  Tous les chiens
      de ses basses-cours composaient une meute dans le besoin ; ses
      palefreniers étaient ses piqueurs; le vicaire du village était
      son grand-aumônier.  Ils l'appelaient tous monseigneur, et ils
      riaient quand il faisait des contes.
    }
  }
  \markuplist {
    \paragraph { % A single-line paragraph may be orphaned
      Madame la ... (may be orphaned)
    }
  }
}
