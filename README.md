# piddif

Convert multiple document formats to Html5-based PDFs.

Note: The "print to PDF" part is missing, use a browser :p

## Synopsis

Org:

```
$ piddif example.org > org.html
```

Markdown:

```
$ piddif --markdown example.md > markdown.html
```

## Development

This project is designed to be developed using `nix`. Install `nix` first.

### Jump into a dev shell

```
nix develop
```

### Build a docker image

Using `nix` it's simple to build a "lean" docker image:

```
nix build .#image
```

I say "lean" because `pandoc` is _huuuge_.

### Running a reloading server

Use `ghcid` to start the server in a reloading mode:

```
ghcid -c 'cabal repl piddif-server' -r
```

Note: This doesn't reload on changes to the static assets like `form.html`.
