# catFlap
RESTful API for catscan documents.

## Building instructions
- Install [stack](https://www.haskellstack.org/), a cross-platform program for developing Haskell projects.
- In the catFlap directory, run `stack build`
- Run catFlap server by `stack exec catFlap`

## Quick development test
To quickly test changes without the full build stack:
- Run `stack ghci`
- Call `main` in ghci. This will start the API server

