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

### Start/stop the gateway

To connect with peers, you'll need to start the `hyper-gateway` with
`M-x hyperdrive-start`. When you're ready to disconnect from the
network, run `M-x hyperdrive-stop`.

### Create a hyperdrive

You can have multiple hyperdrives, each one containing its own set of
files. Use `M-x hyperdrive-new` to create a new hyperdrive from an
`alias`, which can be anything you like. That alias will be combined
with your secret master key, which is generated for you by
`hyper-gateway`, to produce a public key that uniquely identifies that
hyperdrive. `hyperdrive-new` is idempotent since the same alias will
always produce the same public key.

### Open a hyperdrive

You can view the contents of a hyperdrive with `M-x hyperdrive-open`.
While you can always paste in a new full `hyper://` URL,
`hyperdrive-open` remembers the hyperdrives you have already created
or visited, and it will autocomplete those URLs for you.

TODO: Add paragraph on directory view and possible commands

### Write to a hyperdrive

You can write a buffer to a hyperdrive with `hyperdrive-write-buffer`,
which will prompt you for one of hyperdrives you have created as well
as the path in that hyperdrive where you want to store the file. If
you are editing an existing hyperdrive file, `hyperdrive-save-buffer`
will silently update the current hyperdrive entry with the new content.

### Link to a hyperdrive

TODO: Describe org links and regular links

### Share a hyperdrive

Only you can load one of your created hyperdrives by its alias. When
sharing a hyperdrive with someone else, you will need to copy its full
URL with `M-x hyperdrive-copy-url`. With that URL, others can load
files from your hyperdrive directly from your machine or from other
peers who have previously loaded those files from hyperdrive.

TODO: Sharing a link to a particular hyperdrive file

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

## Concepts

TODO: hyperdrives, sparse replication

## Glossary

TODO: files (which aren't really files), directories (which aren't really directories)

## Bugs and Patches

Bugs can be submitted to the [ushin issue
tracker](https://todo.sr.ht/~ushin/ushin). Patches,
comments or questions can be submitted to the [ushin public
inbox](https://lists.sr.ht/~ushin/ushin).

## Acknowledgments

[Adam Porter](https://github.com/alphapapa/) for rewriting
`hyperdrive.el` and for his work on `plz.el`.

[Mauve Signweaver](https://mauve.moe/) for their guidance into the
world of p2p as well as the development of `hyper-gateway`.

[Karl Voit](https://karl-voit.at/) for his feedback, especially the
suggestion that we allow for a non-splitting approach for uploading
files from the filesystem.
