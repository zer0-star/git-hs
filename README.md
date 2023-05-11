# git-hs
A toy git implementetaion written in Haskell

## Implementation status
- [x] `git add`
- [ ] `git commit`
- [ ] `git merge`
- [ ] `git push`
- [ ] `git switch`
- [ ] `git status`
- [ ] `git log`

## Build
### with Nix
```sh
$ nix build
```

### with Cabal
```sh
$ cabal build
```

## Commands
### add

```
git-add - Add file contents to the index

Usage: git-hs add PATH... [-n|--dry-run]

  Add file contents to the index

Available options:
  PATH...                  Files to add
  -n,--dry-run             Don't actually add the files to the index
  -h,--help                Show this help text
```
