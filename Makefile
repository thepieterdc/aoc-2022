advanced-example-%: %
	ghcid $</advanced.hs --setup ":set args $</input.example.txt" --test main

advanced-%: %
	runhaskell $</advanced.hs $</input.txt

simple-example-%: %
	ghcid $</simple.hs --setup ":set args $</input.example.txt" --test main

simple-%: %
	runhaskell $</simple.hs $</input.txt
