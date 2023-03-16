# hyperdrive.el - P2P filesystem in Emacs

[Hyperdrive](https://docs.holepunch.to/building-blocks/hyperdrive) is
a secure, P2P, real-time, local-first, versioned filesystem designed
for easy peer-to-peer file sharing. `hyperdrive.el` is an independent
project built by [USHIN](https://ushin.org) which provides an Emacs
interface for managing hyperdrives.

## Dependencies

### `hyper-gateway`

`hyperdrive.el` relies on
[hyper-gateway](https://github.com/RangerMauve/hyper-gateway/) for
talking to the hypercore network.
[Download](https://github.com/RangerMauve/hyper-gateway/releases) or
compile the binary then ensure that it is executable and in your
`$PATH`. Ensure that `hyperdrive-hyper-gateway-command` is set to the
name you gave to the `hyper-gateway` binary. One way to do this is by
renaming the binary to `hyper-gateway`, the default value for
`hyperdrive-hyper-gateway-command`.

## Installation

The recommended way to install `hyperdrive.el` is with
[quelpa-use-package](https://github.com/quelpa/quelpa-use-package).
To install it, follow the instructions in its documentation; or you
may evaluate the following code:

```
;; Add the MELPA package repository to Emacs's package configuration and refresh the contents.
(cl-pushnew '("melpa" . "https://melpa.org/packages/") package-archives :test #'equal)
(package-refresh-contents)

;; Install and load quelpa-use-package.
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)
```

Then add this form to your `init.el` file and evaluate it:

```
(use-package hyperdrive
  :quelpa (hyperdrive :fetcher git
                      :url "https://git.sr.ht/~ushin/hyperdrive.el")
  :config
  ;; Assuming that the hyper-gateway executable is in "~/.local/bin"
  (add-to-list 'exec-path (expand-file-name "~/.local/bin/")))
```

## Usage

**Be careful what you publish!** Anyone with your public key can
download those shared files from you, your friend, or anyone else who
has them.

### Start/stop the gateway

To connect with peers, you'll need to start the `hyper-gateway` with
`M-x hyperdrive-start`. When you're ready to disconnect from the
network, run `M-x hyperdrive-stop`.

### Create a hyperdrive

You can have multiple hyperdrives, each one containing its own set of
files. Run `M-x hyperdrive-new` then type in a `seed` to create a new
hyperdrive. That seed will be combined with your secret master key,
which is generated for you by `hyper-gateway`, to produce a public key
that uniquely identifies that hyperdrive. `hyperdrive-new` is
idempotent since the same seed will always produce the same public
key. For this reason, a hyperdrive's seed cannot be changed.

### Open a hyperdrive

You can open a hyperdrive folder or file by pasting in a `hyper://`
URL after `M-x hyperdrive-open-url`. Alternatively, `M-x
hyperdrive-find-file` remembers hyperdrives you have already created
or visited. It will prompt you to select a known hyperdrive by its
public key or seed and then enter a path inside that hyperdrive.

The following keybindings are available inside the directory view by
default:

- `n` and `p` move between entries
- `RET` opens file or directory at point
- `^` goes up to the parent directory
- `D` deletes the file or directory (recursively) at point
- `w` copies the URL of the file or directory at point

### Write to a hyperdrive

You can write a buffer to a hyperdrive with `hyperdrive-write-buffer`,
which will prompt you for one of hyperdrives you have created as well
as the path in that hyperdrive where you want to store the file. If
you are editing an existing hyperdrive file, `hyperdrive-save-buffer`
will silently update the current hyperdrive entry with the new content.

### Link to a hyperdrive

In addition to copying the URL at point in the directory view, you can
run `hyperdrive-copy-url` to copy the URL of the current hyperdrive
file or directory. If the current file is an org-mode file,
`org-store-link` will store a link to the hyperdrive file, and if
point is inside a heading, its `CUSTOM_ID`, `ID`, or heading text will
be appended to the stored URL.

### Share a hyperdrive

Only you can load one of your created hyperdrives by its seed. When
sharing a hyperdrive with someone else, you will need to copy its full
URL. Peers can load your hyperdrive files directly from your computer
or from other peers who previously loaded those files.

It is possible to use [DNSLink](https://dnslink.io/) to link to a
hyperdrive with a domain name instead of a public key, like
`hyper://example.org/path/to/file`. Create a TXT record at
`_dnslink.example.org` with the contents `/hyper/<public-key>` (no
trailing slash). Note: relying on DNS adds another point of
centralization, reducing the durability of your link. `hyperdrive.el`
somewhat mitigates this issue by remembering which public key the DNS
record resolved to, so that peers can use the stored public key itself
for subsequent connections.

### Upload files from your filesystem

`hyperdrive-upload-files` lets you upload files from your filesystem
to a hyperdrive. It accepts a `seed`, a `relative-dir`, the directory
on your filesystem which will be the top-level or root directory of
the hyperdrive, and the list of `files` to be shared in the
hyperdrive. `files` can either be a list of filepaths or a function
which returns a list of filepaths.

In order to upload files to a hyperdrive, you must first create it
with `hyperdrive-create-namespace`.

#### Upload a whole directory

`my/hyperdrive-upload-files-foo` uploads everything inside of `~/public/`:

```
(defun my/hyperdrive-upload-files-foo ()
  "Upload all files inside of \"~/public/\" to hyperdrive with seed \"foo\"."
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

TODO: hyperdrives, sparse replication, gateway

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
