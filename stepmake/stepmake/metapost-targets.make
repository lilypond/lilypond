
pfa: mfplain.mem $(PFA_FILES)

mfplain.mem: mfplain.ini
	$(INIMETAPOST) mfplain.ini
