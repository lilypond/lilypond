%
% setup for Request->Element conversion. Guru-only
%
staff_perf =	\requesttranslator {
		Performer "Staff_performer"
		\alias "Staff";

		\contains\requesttranslator{
			Performer "Performer_group_performer"
			\alias "Voice_group";
			\contains\requesttranslator{
				Performer "Performer_group_performer"
				\consists "Note_performer";
			}
		}
		\consists "Key_performer";
 		\consists "Meter_performer";
	}

default_midi_perf = \requesttranslator {
	Performer "Score_performer"
	\alias "Score";

%	\consists "Tempo_performer";

	\contains \requesttranslator{ \staff_perf }
	\contains\requesttranslator{
		Performer "Performer_group_performer"
		\alias "Piano";
		\contains\requesttranslator{\staff_perf}
	}
	\contains\requesttranslator{
		Performer "Staff_performer"
		\alias "Lyric";
		\contains\requesttranslator {
			Performer "Performer_group_performer"
			\contains\requesttranslator{
				Performer "Performer_group_performer"
				\consists "Lyric_performer";
			}
		}
 		\consists "Meter_performer";
	}
	\consists "Swallow_performer";
}

