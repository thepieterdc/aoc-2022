all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid day5/advanced.hs --setup ":set args day5/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid day5/simple.hs --setup ":set args day5/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell day5/advanced.hs day5/input/advanced.txt

run-simple:
	cd ../; runhaskell day5/simple.hs day5/input/simple.txt

pretest:
	mkdir -p test/

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
