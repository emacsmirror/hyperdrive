# hyperdrive.el - Emacs gateway to the Hypercore network

[Hyperdrive](https://docs.holepunch.to/building-blocks/hyperdrive) is
a secure, real-time distributed file system designed for easy
peer-to-peer file sharing. `hyperdrive.el` is an independent project
built by [USHIN](https://ushin.org) which provides an Emacs interface
for managing hyperdrives.

## Installation

### Dependencies

#### `hyper-gateway`

`hyperdrive.el` relies on
[hyper-gateway](https://github.com/RangerMauve/hyper-gateway/) for
talking to the hypercore network.
[Download](https://github.com/RangerMauve/hyper-gateway/releases) or
compile the binary then ensure that it is executable and in your
`$PATH`. Ensure that `hyperdrive-hyper-gateway-command` is set to the
name you gave to the `hyper-gateway` binary. One way to do this is by
renaming the binary to `hyper-gateway`, the default value for
`hyperdrive-hyper-gateway-command`.

#### `plz.el`

`hyperdrive.el` uses [plz.el](https://github.com/alphapapa/plz.el) for
sending HTTP requests to `hyper-gateway`. `plz.el` can be installed
from GNU ELPA with the `package-install` command.

#### `mpv.el`

`hyperdrive.el` uses [mpv.el](https://github.com/kljohann/mpv.el) for
streaming audio and video. `mpv.el` can be installed from GNU ELPA
with the `package-install` command.

#### `compat.el`

`hyperdrive.el` relies on
[compat.el](https://github.com/emacs-compat/compat) to support Emacs
versions prior to Emacs 29. `compat.el` can be installed from GNU ELPA
with the `package-install` command.

### Manual

Clone this repository:

```
git clone https://git.sr.ht/~ushin/hyperdrive.el/ ~/.local/src/hyperdrive.el/
```

Add the following lines to your init.el file:

```
(add-to-list 'load-path "~/.local/src/hyperdrive.el/")
(require 'hyperdrive)
```

## Quickstart

Add the following lines to your `init.el` file:

```
(add-to-list 'load-path "~/.local/src/hyperdrive.el/")
(require 'hyperdrive)
```

Alternatively, with `use-package`:
```
(use-package hyperdrive
  :load-path "~/.local/src/hyperdrive.el/")
```

First, run `M-x hyperdrive-start-gateway` to start `hyper-gateway`.

Next, create a namespace with `M-x hyperdrive-create-namespace`, which
will prompt you for an alias for your namespace. This can be anything
you want, so long it only contains numbers and letters.

Now, put something in your hyperdrive with `M-x
hyperdrive-write-buffer`, which will prompt you for the path where the
current buffer should be stored inside your hyperdrive.

Copy the public key (unique identifier) of the hyperdrive by running
`M-x hyperdrive-public-key`.

Send the public key to a friend who has installed `hyperdrive.el` and
run `M-x hyperdrive-start-gateway`. On your friend's machine, run `M-x
hyperdrive-open` and paste in the public key. Your shared files are
now on your friend's machine!

**Be careful what you publish!** Anyone with your public key can
download those shared files from you, your friend, or anyone else who
has them.

## Usage

### Create a namespace

You can have multiple hyperdrives, where each one contains an isolated
or "namespaced" set of files. Use `M-x hyperdrive-create-namespace` to
create a new namespace.

Each namespaced hyperdrive has an `alias`, the local "petname" given to
a namespaced hyperdrive. An `alias` combines with your secret master
key, which is generated for you by `hyper-gateway`, to produce a public
key which uniquely identifies that hyperdrive. You can load one of your
own hyperdrives with `M-x hyperdrive-load-alias`. Other people cannot
load one of your hyperdrives by its `alias`; they will need its public
key, which you can get with `M-x hyperdrive-public-key`.

### Write a buffer to a hyperdrive

You can write a buffer to a hyperdrive with `hyperdrive-write-buffer`,
which will prompt you for an `alias` and `path` if the current buffer
is not already stored in your hyperdrive. If you are editing an
existing hyperdrive "file", `hyperdrive-write-buffer` will silently
update the current hyperdrive url with the new content.

### Load a hyperdrive

`hyperdrive-load-alias` loads one of your previously created
hyperdrives by prompting you for its `alias`.

To load someone else's hyperdrive or a particular location inside of a
hyperdrive, run `M-x hyperdrive-open` and paste in the URL.

### Upload files from your filesystem

`hyperdrive-upload-files` lets you upload files from your filesystem
to a hyperdrive. It accepts an `alias`, a `relative-dir`, the
directory on your filesystem which will be the top-level or root
directory of the hyperdrive, and the list of `files` to be shared in
the hyperdrive. `files` can either be a list of filepaths or a
function which returns a list of filepaths.

In order to upload files to a hyperdrive, you must first create it
with `hyperdrive-create-namespace`.

#### Upload a whole directory

`my/hyperdrive-upload-files-foo` uploads everything inside of `~/public/`:

```
(defun my/hyperdrive-upload-files-foo ()
  "Upload all files inside of \"~/public/\" to hyperdrive with alias \"foo\"."
  (interactive)
  (hyperdrive-upload-files "foo" "~/public/"
                           (lambda () (directory-files-recursively "~/public/" ""))))
```

#### Upload files by tag

`my/hyperdrive-upload-files-bar` uploads all files inside `~/org/` which
have been tagged as "public" using Protesilaos Stavrou's
[Denote](https://protesilaos.com/emacs/denote) file-naming scheme.

```
(defun my/hyperdrive-upload-files-bar ()
  "Upload all files tagged \"public\" inside of \"~/org/\" to hyperdrive \"bar\"."
  (interactive)
  (hyperdrive-upload-files "foo" "~/"
                           (lambda () (directory-files-recursively "~/org/" ".*_public.*"))))
```

Alternatively, you could select files by tag with Karl Voit's
[filetags](https://github.com/novoid/filetags/). Either way allows for
a "non-splitting" approach where public and private files exist in the
same directory. You can write any function you like to determine which
files to share!

## Bugs and Patches

Bugs can be submitted to the [ushin issue
tracker](https://todo.sr.ht/~ushin/ushin). Patches,
comments or questions can be submitted to the [ushin public
inbox](https://lists.sr.ht/~ushin/ushin).

## Acknowledgments

[Mauve Signweaver](https://mauve.moe/) for their guidance into the
world of p2p as well as the development of `hyper-gateway`.

[Adam Porter](https://github.com/alphapapa/) for his feedback on the
structure and design of `hyperdrive.el` as well as the development of
`plz.el`.

[Karl Voit](https://karl-voit.at/) for his feedback, especially the
suggestion that we allow for a non-splitting approach which avoids
unnecessary borders within topics.
