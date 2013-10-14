Chess
=====

A few minor extensions to a [learning exercise on the
haskellwiki][haskellwiki]. There is only a minor change to the original
implementation to make it compile with GHC. And some additional modules have
been added.

The engine does not support double square pawn advances or castling and no
support has yet been added. 

Some things have been added, amateurishly: 

* Print a game state (turn, board and possible moves)
* Parse a game state (and an optional move command) 
* Command line state parser / transformer
* ØMQ binding
    
I'm using this to learn some Haskell and to work as a chess engine for a chess
server. 

This is not intended to be and efficient implementation and Parsing into FEN or
something might be better but this is for fun and learning.

The ØMQ server has a on the ØMQ and the [Haskell bindings][haskellzeromq]

To compile: 

    make             

Sample format:

    $ cat sample/invalid_move.txt
    White
    BR BN BB BQ BK BB BN BR
    BP BP BP BP BP BP BP BP
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    WP WP WP WP WP WP WP WP
    WR WN WB WQ WK WB WN WR

It can generate moves:

    cat sample/gen_moves.txt | ./bin/cli
    White
    BR BN BB BQ BK BB BN BR
    BP BP BP BP BP BP BP BP
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    WP WP WP WP WP WP WP WP
    WR WN WB WQ WK WB WN WR

    WP a2:a3 WP b2:b3 WP c2:c3 WP d2:d3 WP e2:e3 WP f2:f3 WP g2:g3 WP h2:h3 WN b1:c3 WN b1:a3 WN g1:h3 WN g1:f3WP a2:a3 WP b2:b3 WP c2:c3 WP d2:d3 WP e2:e3 WP f2:f3 WP g2:g3 WP h2:h3 WN b1:c3 WN b1:a3 WN g1:h3 WN g1:f3

It parses a state and a move action and returns the updated state (the move list can be omitted):

    cat sample/make_move.txt
    White
    BR BN BB BQ BK BB BN BR
    BP BP BP BP BP BP BP BP
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    WP WP WP WP WP WP WP WP
    WR WN WB WQ WK WB WN WR

    WP a2:a3 WP b2:b3 WP c2:c3 WP d2:d3 WP e2:e3 WP f2:f3 WP g2:g3 WP h2:h3 WN b1:c3 WN b1:a3 WN g1:h3 WN g1:f3WP a2:a3 WP b2:b3 WP c2:c3 WP d2:d3 WP e2:e3 WP f2:f3 WP g2:g3 WP h2:h3 WN b1:c3 WN b1:a3 WN g1:h3 WN g1:f3

    action: WP e2:e3

Returns updated board, the turns and the possible moves.

    $ cat sample/make_move.txt | ./bin/cli
    Black
    BR BN BB BQ BK BB BN BR
    BP BP BP BP BP BP BP BP
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    -- -- -- -- -- -- -- --
    -- -- -- -- WP -- -- --
    WP WP WP WP -- WP WP WP
    WR WN WB WQ WK WB WN WR

    BN b8:c6 BN b8:a6 BN g8:h6 BN g8:f6 BP a7:a6 BP b7:b6 BP c7:c6 BP d7:d6 BP e7:e6 BP f7:f6 BP g7:g6 BP h7:h6

It only accepts valid moves (moves which are in the list generated):

    $ cat sample/invalid_move.txt | ./bin/cli
    Invalid Move

Everything else it doesn't understand it returns a parse error, it doesn't test
the validity of the game state, it will parse a board full of bishops:

    $ echo "BK WZ KK" | ./bin/cli
    Parse Error

The server can be started to listen for connections:

    $ ./bin/server
    Starting Server

Then the test client will send `./sample/make_move.txt`, 10 times:

    $ ./bin/client
    ..

[haskellwiki]: http://www.haskell.org/haskellwiki/Learning_Haskell_with_Chess 
[haskellzeromq]: http://zeromq.org/bindings:haskell
