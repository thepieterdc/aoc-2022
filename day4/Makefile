all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid day4/advanced.hs --setup ":set args day4/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid day4/simple.hs --setup ":set args day4/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell day4/advanced.hs day4/input/advanced.txt

run-simple:
	cd ../; runhaskell day4/simple.hs day4/input/simple.txt

pretest:
	mkdir -p test/

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
