make all: dev

dev:
	elm-live src/Main.elm --open -S -h 0.0.0.0 -- --output=index.js --debug
prod:
	elm make src/Main.elm --optimize

