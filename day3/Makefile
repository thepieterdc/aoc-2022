all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid day3/advanced.hs --setup ":set args day3/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid day3/simple.hs --setup ":set args day3/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell day3/advanced.hs day3/input/advanced.txt

run-simple:
	cd ../; runhaskell day3/simple.hs day3/input/simple.txt

pretest:
	mkdir -p test/

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
