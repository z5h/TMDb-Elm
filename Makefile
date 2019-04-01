export NODE_BIN := node_modules/.bin
export ELM_LIVE = $(NODE_BIN)/elm-live

live:
	$(ELM_LIVE) src/Main.elm --open --pushstate --dir=web -- --output=web/elm.js
