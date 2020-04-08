make all: dev

dev:
	elm-live src/Main.elm --open -S -h 0.0.0.0 -d public -- --output=public/js/index.js --debug
prod:
	elm make src/Main.elm --optimize --output=public/js/index.js

