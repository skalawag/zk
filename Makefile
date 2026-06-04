.PHONY: test clean

test:
	emacs -Q --batch -L . -l tests/zk-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -rf .test-notes
