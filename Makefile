.PHONY: run wbuild wfmt test

run:
	elm make src/Day2.elm --output=main.js
	cat src/Day2.example.txt | node ./cli.js example
	cat src/Day2.input.txt | node ./cli.js input

wbuild:
	onchange 'src/*.elm' -- make

wfmt:
	onchange 'src/*.elm' -- elm-format --yes {{file}}

test:
	elm-test tests/Day2.elm
