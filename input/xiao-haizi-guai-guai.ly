%% xiao-haizi-guai-guai.ly
%%
%% (xiao3: small,
%%  hai2zi5: child,
%%  guai1-guai1: well-behaved)

\version "2.11.51"

\header {
  title = "小孩子乖乖"
  subtitle = "(Xiao3 hai2zi5 guai1 guai1)"
}

<<
  \relative c'' {
    %% Beams are melismata, no autobeams.
    \set Staff.autoBeaming = ##f

    | g4 c8 a g4 g
    | e4 g8 a g4 g
    | a4 g8 e d4 d
    | e4 g8[ e] d[ e] c4
    
    | a'8 g a g e a g4
    | e8 g e d c4 r
    | a8 c d e c4 r \bar "|."
  }
  \addlyrics {
    小 孩 子 乖 乖
    把 門 兒 開 開
    快 點 兒 開 開
    我 要 進 來
    
    不 開 不 開 不 能 開
    你 是 大 野 狼
    不 讓 你 進 來
  }
>>

%%% Local Variables:
%%% coding: utf-8
%%% End:
