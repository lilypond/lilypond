\version "1.3.146"

%non of the dynamics properties work anymore

\score { 
  \context Voice \notes\relative c {
%        \stemDown\slurDown\tieDown

        \property Voice.dynamicDirection = #1
        \property Voice.dynamicPadding = #40
        c \p c \<  \! c \ff\> c \!c-\p 

  }
}
