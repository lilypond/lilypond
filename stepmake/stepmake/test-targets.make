.PHONY: check test

check: test

test: $(TEST_EXECUTABLE)
	$(TEST_EXECUTABLE)
