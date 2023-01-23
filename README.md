# hyperdrive.el - Emacs gateway to the Hypercore network

## Installation

### Dependencies

#### `hyper-gateway`

`hyperdrive.el` relies on
[hyper-gateway](https://github.com/RangerMauve/hyper-gateway/) for
talking to the hypercore network.
[Download](https://github.com/RangerMauve/hyper-gateway/releases) or
compile the binary then ensure that it is executable and in your
`$PATH`.

#### `plz.el`

`hyperdrive.el` uses [plz.el](https://github.com/alphapapa/plz.el) for
sending HTTP requests to `hyper-gateway`. `plz.el` can be installed
from GNU ELPA with `package-install` command.

### Manual

Clone this repository:

```
git clone https://git.sr.ht/~breatheoutbreathein/hyperdrive.el/ ~/.local/src/hyperdrive.el/
```

Add the following lines to your init.el file:

```
(add-to-list 'load-path "~/.local/src/hyperdrive.el/")
(require 'hyperdrive)
```

Ensure that `hyper-gateway-command` is set to the name you gave to the
`hyper-gateway` binary.

## Quickstart

Follow these instructions to get started with a sync directory inside
`~/public/`.

Add the following lines to your `init.el` file:

```
(add-to-list 'load-path "~/.local/src/hyperdrive.el/")
(require 'hyperdrive)
(setq hyperdrive-namespaces
      `(,(hyperdrive-namespace-create
          :alias "foo" ; Human-readable alias
          :relative-dir "~/public/" ; Top-level directory of hyperdrive
          :files (lambda () (directory-files-recursively "~/public/" ""))))) ; Files to share
```

Alternatively, with `use-package`:
```
(use-package hyperdrive
  :load-path "~/.local/src/hyperdrive.el/"
  :config
  (setq hyperdrive-namespaces
        `(,(hyperdrive-namespace-create
            :alias "default" ; Human-readable alias
            :relative-dir "~/public/" ; Top-level directory of hyperdrive
            :files (lambda () (directory-files-recursively "~/public/" "")))))) ; Files to share
```

First, run `M-x hyperdrive-start-gateway` to start `hyper-gateway`.

Add some files you want to share into `~/public/`. Sync this directory
with the hyperdrive with `M-x hyperdrive-sync-shared-files`. Copy the
public key (unique identifier) of the hyperdrive by running `M-x
hyperdrive-public-key`.

Send the public key to a friend who has installed `hyperdrive.el` and run
`M-x hyperdrive-start-gateway`. On your friend's machine, run `M-x
hyperdrive-load-url` and paste in the public key. Your shared files are
now on your friend's machine!

**Be careful what you publish!** Anyone with your public key can
download those shared files from you, your friend, or anyone else who
has them.

## Usage

### Namespaces

[Hyperdrive](https://docs.holepunch.to/building-blocks/hyperdrive) is
a secure, real-time distributed file system designed for easy P2P file
sharing. You can have multiple hyperdrives, where each one contains an
isolated or "namespaced" set of files.

Use `hyperdrive-namespace-create` to create a namespace:

```
(setq hyperdrive-namespaces
      `(,(hyperdrive-namespace-create
          :alias "foo"
          :relative-dir "~/public/"
          :files (lambda () (directory-files-recursively "~/public/" "")))
        ,(hyperdrive-namespace-create
          :alias "bar"
          :relative-dir "~/"
          :files (lambda () (directory-files-recursively "~/" ".*_public.*")))))
```

This sets up two hyperdrives. The "foo" hyperdrive, just like in the
Quickstart, syncs everything inside `~/public/`. The "bar" hyperdrive
syncs all files inside `~/` which have been tagged as "public" using
Protesilaos Stavrou's [Denote](https://protesilaos.com/emacs/denote)
file-naming scheme. Alternatively, you could select files by tag with
Karl Voit's [filetags](https://github.com/novoid/filetags/). Either
way allows for a "non-splitting" approach where public and private
files exist in the same directory. You can write any function you like
to determine which files to share!

- `:alias` refers to the local "petname" given to a namespaced
  hyperdrive. An `:alias` combines with your secret master key, which
  is generated for you by `hyper-gateway`, to produce a public key
  which uniquely identifies that hyperdrive. You can load files from
  one of your own hyperdrives by passing its `:alias` to `M-x
  hyperdrive-load-url`, either as `hyper://foo` or simply `foo`. Other
  people cannot load one of your hyperdrives by its alias; they will
  need its public key, which you can get with `M-x hyperdrive-public-key`.
  There is a one-to-one mapping between an `:alias` and a hyperdrive,
  which means that you cannot use the same name twice.

- `:relative-dir` refers to the directory on your filesystem which
  will be the top-level or root directory of the hyperdrive.

- `:files` refers to the files to be shared in the hyperdrive.
  `:files` can either be a list of filepaths or a function which
  returns a list of filepaths.

## Bugs and Patches

Bugs can be submitted to the [ushin issue
tracker](https://todo.sr.ht/~breatheoutbreathein/ushin). Patches,
comments or questions can be submitted to the [ushin public
inbox](https://lists.sr.ht/~breatheoutbreathein/ushin).

## Acknowledgments

[Karl Voit](https://karl-voit.at/) for his feedback, especially the
suggestion that we allow for a non-splitting approach which avoids
unnecessary borders within topics.
