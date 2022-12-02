all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid day2/advanced.hs --setup ":set args day2/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid day2/simple.hs --setup ":set args day2/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell day2/advanced.hs day2/input/advanced.txt

run-simple:
	cd ../; runhaskell day2/simple.hs day2/input/simple.txt

pretest:
	mkdir -p test/

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
