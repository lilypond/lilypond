  \header {
        title = "Two miniatures";
  }
  
  #(set! point-and-click #t)
  
  \paper { linewidth = -1.0; }

  \score {
    \notes { c'4 d'4 }
    \header {
    	opus = "Opus 1.";
	piece = "Up";
    }
  }
  
  \score {
    \notes { d'4 c'4  }
    \header {
    	opus = "Opus 2.";
	piece = "Down";
    }
  }
