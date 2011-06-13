# Generate the docs and copy them to the website dir
haddock:
	cabal haddock --hyperlink-source
	rm -rf web/reference/
	cp -r dist/doc/html/hakyll/ web/reference/

# Run the tests
test:
	runghc -isrc -itests tests/TestSuite.hs
