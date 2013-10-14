.PHONY: cli server runserver client

compile: server cli client

server: bindir 
	ghc -ihsChess -o bin/server Server.hs

cli:  bindir
	ghc -ihsChess -o bin/cli Cli.hs

client: bindir
	ghc -ihsChess -o bin/client TestClient.hs

clean:
	rm -rf bin

bindir:
	mkdir -p bin

runserver: compile
	./bin/server


