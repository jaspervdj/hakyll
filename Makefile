PACKAGE="$(shell stack ls dependencies --separator='-' | grep hakyll)"
LOCAL_DOC_ROOT="$(shell stack path --local-doc-root)"

# Generate the docs and copy them to the website dir
haddock:
	stack build --haddock --no-haddock-deps
	rsync -r "$(LOCAL_DOC_ROOT)/$(PACKAGE)/" web/reference/

.PHONY: haddock
