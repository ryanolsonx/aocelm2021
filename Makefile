.PHONY: run wbuild wfmt test

run:
	elm make src/Day1.elm --optimize --output=main.js
	cat src/Day1.example.txt | node ./cli.js example
	cat src/Day1.input.txt | node ./cli.js input

wbuild:
	onchange 'src/*.elm' -- make

wfmt:
	onchange 'src/*.elm' -- elm-format --yes {{file}}

test:
	elm-test tests/Day1.elm