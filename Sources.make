# -*- Makefile-*-
# sourcefiles to be shipped. Also used for dependencies

hdr=   bar.hh barreg.hh beam.hh boxes.hh break.hh clefreg.hh clefitem.hh\
	colhpos.hh  commandrequest.hh  complexcolumn.hh \
	complexwalker.hh complexstaff.hh\
	const.hh debug.hh dimen.hh directionalspanner.hh\
	 glob.hh  grouping.hh headreg.hh idealspacing.hh\
	identifier.hh identparent.hh \
	inputmusic.hh inputscore.hh inputstaff.hh item.hh key.hh keyitem.hh\
	keyreg.hh\
	keyword.hh leastsquares.hh lexer.hh linespace.hh \
	localkeyitem.hh localkeyreg.hh lookup.hh \
	lyriccolumn.hh  lyricitem.hh lyricstaff.hh lyricwalker.hh\
	main.hh  meter.hh meterreg.hh misc.hh\
	molecule.hh moment.hh musicalrequest.hh\
	notehead.hh notename.hh offset.hh paperdef.hh\
	parseconstruct.hh pcol.hh proto.hh pscore.hh pstaff.hh qlp.hh\
	qlpsolve.hh register.hh reqtodo.hh request.hh rest.hh \
	sccol.hh score.hh\
	scoreline.hh scorewalker.hh script.hh scriptdef.hh scriptreg.hh \
	slur.hh slurreg.hh source.hh sourcefile.hh\
	spanner.hh staff.hh\
	staffelem.hh staffeleminfo.hh staffline.hh staffsym.hh stembeamreg.hh\
	stcol.hh stem.hh staffwalker.hh symbol.hh symtable.hh\
	tex.hh textdef.hh \
	textitem.hh textreg.hh textspanner.hh timedescription.hh \
	tstream.hh voice.hh\
	voicegroup.hh

mycc=bar.cc barreg.cc beam.cc boxes.cc break.cc calcideal.cc clefreg.cc\
	clefitem.cc colhpos.cc  commandrequest.cc\
	complexcolumn.cc complexstaff.cc complexwalker.cc \
	debug.cc dimen.cc\
	directionalspanner.cc\
	 grouping.cc groupregs.cc headreg.cc\
	idealspacing.cc identifier.cc\
	 inputmusic.cc inputscore.cc\
	inputstaff.cc item.cc key.cc keyitem.cc \
	keyreg.cc keyword.cc\
	leastsquares.cc lexerinit.cc linespace.cc \
	localkeyitem.cc localkeyreg.cc lookup.cc\
	lyriccolumn.cc lyricitem.cc lyricstaff.cc lyricwalker.cc\
	main.cc  meter.cc meterreg.cc\
	misc.cc molecule.cc mylexer.cc note.cc notehead.cc  notename.cc\
	paperdef.cc pcol.cc pscore.cc pstaff.cc qlp.cc qlpsolve.cc\
	register.cc request.cc rest.cc  sccol.cc score.cc\
	scoreline.cc scores.cc scorewalker.cc script.cc\
	scriptdef.cc scriptreg.cc slur.cc\
	slurreg.cc source.cc sourcefile.cc\
	spanner.cc staff.cc\
	staffelem.cc staffline.cc staffsym.cc\
	stembeamreg.cc stcol.cc stem.cc\
	staffwalker.cc symbol.cc symtable.cc  tex.cc texbeam.cc\
	texslur.cc textdef.cc textitem.cc textreg.cc textspanner.cc\
	timedescription.cc tstream.cc voice.cc voiceelt.cc \
	voiceregs.cc voicegroup.cc\
	warn.cc windhoos-suck-suck-suck-thank-you-cygnus.cc wordwrap.cc\
	template1.cc template2.cc template3.cc template4.cc\
	template5.cc template6.cc version.cc

# a bit of a hack to keep exec size under control.
stablecc=request.cc bar.cc boxes.cc break.cc  \
	item.cc keyword.cc leastsquares.cc \
	 lookup.cc molecule.cc meter.cc\
	paperdef.cc parser.cc lexer.cc pstaff.cc qlp.cc qlpsolve.cc\
	template1.cc template2.cc template3.cc template4.cc\
	template5.cc template6.cc version.cc tstream.cc  tex.cc\
	voice.cc wordwrap.cc spanner.cc 
