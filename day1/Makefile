all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid day1/advanced.hs --setup ":set args day1/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid day1/simple.hs --setup ":set args day1/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell day1/advanced.hs day1/input/advanced.txt

run-simple:
	cd ../; runhaskell day1/simple.hs day1/input/simple.txt

test-advanced:
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple:
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
