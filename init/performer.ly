%
% setup for Request->Element conversion. Guru-only
%

\requesttranslator {
	Performer "Score_performer"
	\alias "Score";
%	\consists "Clef_performer";
%	\consists "Key_performer";
%	\consists "Meter_performer";
%	\consists "Tempo_performer";

	\contains\requesttranslator {
		Performer "Staff_performer"
		\alias "Midi";
		\contains\requesttranslator{
			Performer "Voice_group_performer"
			\alias "Voice_group";
			\contains\requesttranslator{
				Performer "Performer_group_performer"
				\consists "Lyric_performer";
				\consists "Note_performer";
 				
			}
		}
		\consists "Key_performer";
 		\consists "Meter_performer";
		\consists "Swallow_performer";
	}
}
