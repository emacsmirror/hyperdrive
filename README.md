# hyperdrive.el - P2P filesystem in Emacs

[Hyperdrive](https://docs.holepunch.to/building-blocks/hyperdrive) is
a secure, P2P, real-time, local-first, versioned filesystem designed
for easy peer-to-peer file sharing. `hyperdrive.el` is an independent
project built by [USHIN](https://ushin.org) which provides an Emacs
interface for managing hyperdrives.

hyperdrive.el is in early development. If something breaks, please see
the [troubleshooting section](#troubleshooting)

## Installation

`hyperdrive.el` is available on
[MELPA](https://melpa.org/#/getting-started). Once you've set up
MELPA, you can run `M-x package-install` then enter `hyperdrive`.

### `hyper-gateway`

`hyperdrive.el` relies on
[hyper-gateway](https://github.com/RangerMauve/hyper-gateway/) for
talking to the hypercore network ([installation
instructions](https://github.com/RangerMauve/hyper-gateway#how-do-i-install-hyper-gateway)).

## Usage

**Be careful what you publish!** Anyone with your public key can
download your published files from you, your friend, or anyone else who
has them.

### Start/stop the gateway

To connect with peers, you'll need to start `hyper-gateway`. If you
[install `hyper-gateway` as a SystemD
service](https://github.com/RangerMauve/hyper-gateway#how-do-i-run-hyper-gateway-as-a-background-process-on-gnulinux--systemd),
you can connect and disconnect from the network with `M-x
hyperdrive-start` and `M-x hyperdrive-stop`. Otherwise, follow [these
instructions](https://github.com/RangerMauve/hyper-gateway#usage) to
run `hyper-gateway` manually.

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

TODO: Uploading files from the filesystem has not been reimplemented since the upgrade to the latest version of hyper-gateway.

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

### Hyper-gateway

[Hyper-gateway](https://github.com/RangerMauve/hyper-gateway/) handles
interactions with hyperdrive under the hood, and it runs a local HTTP
server which accepts HTTP requests. In `hyperdrive.el`, P2P
interactions consist mostly of, e.g., `GET` requests to download files
and `PUT` requests to write files to a hyperdrive.

### Sparse replication

[Hyperdrive](https://docs.holepunch.to/building-blocks/hyperdrive) is
sparsely replicated, meaning that peers can download particular files
from a hyperdrive without having to get the whole drive. This reduces
both load times and disk usage.

### Hyperdrive entries

Instead of files and folders, Hyperdrive has entries and entry
prefixes. It's possible for a "file" entry and a "folder" entry prefix
to have the same name, like `hyper://<public-key>/path/to/` (a
"directory") and `hyper://<public-key>/path/to` (a "file"). In this
case, the directory listing for `hyper://<public-key>/path/` would
display the `to` entry but not the `to/` entry prefix. Because entry
prefixes only exist when they prefix an entry, deleting the last
"file" in a "directory" causes the "directory" to disappear as well.
When a hyperdrive "file" or "directory" is not found, `hyperdrive.el`
prompts to go to the parent "directory."

## Troubleshooting

If you run into issues, please first try resetting the value of
`hyperdrive-hyperdrives`:

```
(progn
  (setf hyperdrive-hyperdrives (make-hash-table :test #'equal))
  (persist-save 'hyperdrive-hyperdrives))
```

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
