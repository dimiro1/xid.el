# xid.el

**xid.el** is an Emacs Lisp implementation of the [XID specification](https://github.com/rs/xid). These identifiers are:

- **Compact**: 12 bytes (96 bits) in length.
- **Readable**: Base32 encoded for ease of use.
- **Sortable**: Sortable by creation time.
- **Unique**: Contain embedded creation time, machine ID, process ID, and a counter.

## Installation

Clone the repository and load it in your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/xid.el")
(require 'xid)
```


Alternatively, use straight.el:

```elisp
(straight-use-package
 '(xid :type git :host github :repo "dimiro1/xid.el"))
```

Or straight.el + use-package macro

```elisp
(use-package xid
  :straight (:type git :host github :repo "dimiro1/xid.el")
  :bind ("C-c x i d" . xid-insert))
```

## Usage

- Generate a new XID: `(xid-generate-encoded)`
- Insert XID at point: `M-x xid-insert`
- Copy XID to clipboard: `M-x xid-copy-to-clipboard`

## Acknowledgements

This implementation is heavily inspired by the [Go implementation of XID](https://github.com/rs/xid).
The Emacs Lisp version mirrors the logic and structure of the original Go code for compatibility and correctness.

