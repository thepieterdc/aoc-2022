all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid Day18/Advanced.hs --setup ":set args Day18/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day18/Simple.hs --setup ":set args Day18/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell Day18/Advanced.hs Day18/input/advanced.txt

run-simple:
	cd ../; runhaskell Day18/Simple.hs Day18/input/simple.txt

pretest:
	mkdir -p test/

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
