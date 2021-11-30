.PHONY: build fast fmt

build:
	elm make src/Day1.elm

fmt:
	elm-format --yes src/Day1.elm
	elm-format --yes tests/Day1.elm