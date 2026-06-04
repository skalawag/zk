# zk.el

`zk.el` is a lightweight Emacs package for luhmann-style slip box note
workflow on top of `org-mode`.

The package is file-first. Drafts are captured quickly into an inbox
with generated filenames. Permanent notes use Luhmann-style addresses
as canonical filenames, e.g. `1a2.org`, mirrored inside the file as
`#+address: 1a2`. Notes still get ordinary org IDs in `:ID:`, but links use the Luhmann address, not the org ID.

Addresses are normalized to lowercase and must match `[0-9]+[a-z0-9]*`. Duplicate addresses are rejected. Once assigned, addresses are stable by default; changing one is a deliberate `zk-readdress` operation that rewrites incoming `zk:` links.

## V1 workflow

- `M-x zk-capture` --- prompt for a draft slug, create an inbox note, and open it for writing.
- `M-x zk-refile` --- promote the draft by assigning an address.
- `M-x zk-new` --- create a permanent addressed note directly.
- `M-x zk-readdress` --- deliberately change an existing address and rewrite incoming `zk:` links.
- `M-x zk-open` / `M-x zk-find` --- open a note by completion.
- `M-x zk-search` --- search note contents.
- `M-x zk-link` --- insert an explicit `zk:` address link to a note; candidates are address-first, e.g. `1a2.org --- Title`; link description defaults to the target title.
- `M-x zk-update-id-locations` --- rescan zk notes for Org ID registration.
- `M-x zk-review` --- inspect notes needing attention.
- `M-x zk-dispatch` --- command hub.

## Local testing

Run the test suite:

```sh
make test
```

Open a clean Emacs with the local package loaded:

```sh
emacs -Q -L . -l test-init.el
```

`test-init.el` stores the local Org ID cache at `notes/.org-id-locations`. `zk:` links resolve by address/filename, so they do not require Org ID lookup.
