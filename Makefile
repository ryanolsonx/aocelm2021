.PHONY: build fmt test

build:
	elm make src/Day1.elm

prod:
	elm make --optimize src/Day1.elm

fmt:
	elm-format --yes src/Day1.elm
	# elm-format --yes tests/Day1.elm

test:
	elm-test tests/Day1.elm