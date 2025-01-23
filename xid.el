;;; xid.el --- Globally unique ID generator -*- lexical-binding: t -*-

;; Author: Claudemiro Alves Feitosa Neto
;; Version: 1.4
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/dimiro1/xid.el
;; License: MIT

;;; Commentary:

;; An implementation of the XID specification (https://github.com/rs/xid) for Emacs.
;; XIDs are globally unique identifiers that are:
;; - 12 bytes in length (96 bits)
;; - Base32 encoded for readability
;; - Sortable by creation time
;; - Contain embedded creation time, machine ID, process ID, and counter

;; Each XID consists of:
;; - 4-byte value representing seconds since the Unix epoch
;; - 3-byte machine identifier
;; - 2-byte process ID
;; - 3-byte counter starting with a random value

;; Basic usage:
;;   (require 'xid)
;;   (xid-generate-encoded)         ; => "cbf5rgspdr1ak2u5qcs0"
;;   M-x xid-insert                 ; Inserts a new XID at point

;;; Code:

(defgroup xid nil
  "XID globally unique ID generator.
XIDs are sortable, timestamped, unique identifiers based on MongoDB's ObjectID."
  :group 'tools)

(defcustom xid-initial-counter (random (lsh 1 24))
  "Initial value for the XID counter."
  :type 'integer
  :group 'xid)

(defconst xid-encoded-len 20
  "Length of base32 encoded XID string.")

(defconst xid-raw-len 12
  "Length of raw XID bytes (4+3+2+3).")

(defconst xid-base32-encoding "0123456789abcdefghijklmnopqrstuv"
  "Base32 character set used for XID encoding.
This specific character set is chosen to maintain sortability.")

(defvar xid-machine-id
  (let* ((id (make-vector 3 0))
         (hostname (or (getenv "HOSTNAME") (system-name) "localhost"))
         (hash (sha1 (encode-coding-string hostname 'utf-8))))
    (dotimes (i 3)
      (aset id i (aref hash i)))
    id)
  "Machine ID for XID generation.
Generated from SHA1 hash of hostname, using first 3 bytes.")

(defvar xid-process-id
  (logand (emacs-pid) #xFFFF)
  "Process ID for XID generation (16-bit value).
Derived from current Emacs process ID.")

(defvar xid-counter xid-initial-counter
  "Counter for XID generation, starts at a customizable random value.
Increments for each generated XID, wraps around at 24 bits.")

(defsubst xid--encode-byte (byte shift mask)
  "Encode BYTE shifted by SHIFT and masked with MASK using base32.
Internal function used by `xid-encode'."
  (aref xid-base32-encoding (logand (ash byte shift) mask)))

(defun xid--decode-byte (char)
  "Get the numeric value of Base32 character CHAR."
  (let ((pos (cl-position char xid-base32-encoding)))
    (if pos
        pos
      (error "Invalid Base32 character: %c" char))))

;; Based on: https://github.com/rs/xid/blob/master/id.go#L259
(defun xid-decode (encoded-xid)
  "Decode a 20-character Base32 XID string into a 12-byte raw XID."
  (when (/= (length encoded-xid) xid-encoded-len)
    (error "Encoded XID must be exactly %d characters" xid-encoded-len))
  (let ((raw-id (make-string xid-raw-len 0)))
    ;; Decode characters into bytes
    (aset raw-id  0 (logior (ash (xid--decode-byte (aref encoded-xid 0)) 3)
                            (ash (xid--decode-byte (aref encoded-xid 1)) -2)))
    (aset raw-id  1 (logior (ash (xid--decode-byte (aref encoded-xid 1)) 6)
                            (ash (xid--decode-byte (aref encoded-xid 2)) 1)
                            (ash (xid--decode-byte (aref encoded-xid 3)) -4)))
    (aset raw-id  2 (logior (ash (xid--decode-byte (aref encoded-xid 3)) 4)
                            (ash (xid--decode-byte (aref encoded-xid 4)) -1)))
    (aset raw-id  3 (logior (ash (xid--decode-byte (aref encoded-xid 4)) 7)
                            (ash (xid--decode-byte (aref encoded-xid 5)) 2)
                            (ash (xid--decode-byte (aref encoded-xid 6)) -3)))
    (aset raw-id  4 (logior (ash (xid--decode-byte (aref encoded-xid 6)) 5)
                            (xid--decode-byte (aref encoded-xid 7))))
    (aset raw-id  5 (logior (ash (xid--decode-byte (aref encoded-xid 8)) 3)
                            (ash (xid--decode-byte (aref encoded-xid 9)) -2)))
    (aset raw-id  6 (logior (ash (xid--decode-byte (aref encoded-xid 9)) 6)
                            (ash (xid--decode-byte (aref encoded-xid 10)) 1)
                            (ash (xid--decode-byte (aref encoded-xid 11)) -4)))
    (aset raw-id  7 (logior (ash (xid--decode-byte (aref encoded-xid 11)) 4)
                            (ash (xid--decode-byte (aref encoded-xid 12)) -1)))
    (aset raw-id  8 (logior (ash (xid--decode-byte (aref encoded-xid 12)) 7)
                            (ash (xid--decode-byte (aref encoded-xid 13)) 2)
                            (ash (xid--decode-byte (aref encoded-xid 14)) -3)))
    (aset raw-id  9 (logior (ash (xid--decode-byte (aref encoded-xid 14)) 5)
                            (xid--decode-byte (aref encoded-xid 15))))
    (aset raw-id 10 (logior (ash (xid--decode-byte (aref encoded-xid 16)) 3)
                            (ash (xid--decode-byte (aref encoded-xid 17)) -2)))
    (aset raw-id 11 (logior (ash (xid--decode-byte (aref encoded-xid 17)) 6)
                            (ash (xid--decode-byte (aref encoded-xid 18)) 1)
                            (ash (xid--decode-byte (aref encoded-xid 19)) -4)))
    ;; Validate last character
    (when (/= (aref xid-base32-encoding (logand (ash (aref raw-id 11) 4) #x1f))
              (aref encoded-xid 19))
      (error "Invalid encoded string: checksum failed"))
    raw-id))

;; Based on: https://github.com/rs/xid/blob/master/id.go#L201
(defun xid-encode (id)
  "Encode a 12-byte raw XID into a 20-character Base32 string."
  (when (/= (length id) xid-raw-len)
    (error "ID must be exactly %d bytes" xid-raw-len))
  (let ((dst (make-string xid-encoded-len 0)))
    ;; Go-like encoding sequence
    (aset dst  0 (xid--encode-byte (ash (aref id 0) -3) 0 #x1f))
    (aset dst  1 (xid--encode-byte (logior (ash (aref id 0) 2) (ash (aref id 1) -6)) 0 #x1f))
    (aset dst  2 (xid--encode-byte (ash (aref id 1) -1) 0 #x1f))
    (aset dst  3 (xid--encode-byte (logior (ash (aref id 1) 4) (ash (aref id 2) -4)) 0 #x1f))
    (aset dst  4 (xid--encode-byte (logior (ash (aref id 2) 1) (ash (aref id 3) -7)) 0 #x1f))
    (aset dst  5 (xid--encode-byte (ash (aref id 3) -2) 0 #x1f))
    (aset dst  6 (xid--encode-byte (logior (ash (aref id 3) 3) (ash (aref id 4) -5)) 0 #x1f))
    (aset dst  7 (xid--encode-byte (logand (aref id 4) #x1f) 0 #x1f))
    (aset dst  8 (xid--encode-byte (ash (aref id 5) -3) 0 #x1f))
    (aset dst  9 (xid--encode-byte (logior (ash (aref id 5) 2) (ash (aref id 6) -6)) 0 #x1f))
    (aset dst 10 (xid--encode-byte (ash (aref id 6) -1) 0 #x1f))
    (aset dst 11 (xid--encode-byte (logior (ash (aref id 6) 4) (ash (aref id 7) -4)) 0 #x1f))
    (aset dst 12 (xid--encode-byte (logior (ash (aref id 7) 1) (ash (aref id 8) -7)) 0 #x1f))
    (aset dst 13 (xid--encode-byte (ash (aref id 8) -2) 0 #x1f))
    (aset dst 14 (xid--encode-byte (logior (ash (aref id 8) 3) (ash (aref id 9) -5)) 0 #x1f))
    (aset dst 15 (xid--encode-byte (logand (aref id 9) #x1f) 0 #x1f))
    (aset dst 16 (xid--encode-byte (ash (aref id 10) -3) 0 #x1f))
    (aset dst 17 (xid--encode-byte (logior (ash (aref id 10) 2) (ash (aref id 11) -6)) 0 #x1f))
    (aset dst 18 (xid--encode-byte (ash (aref id 11) -1) 0 #x1f))
    (aset dst 19 (xid--encode-byte (logand (ash (aref id 11) 4) #x1f) 0 #x1f))
    dst))


(defun xid-generate-with-timestamp (timestamp)
  "Generate a new XID as raw bytes using given TIMESTAMP.
TIMESTAMP should be a Unix timestamp (seconds since epoch).
Returns a 12-byte string containing the raw XID components."
  (unless (and (integerp timestamp) (>= timestamp 0))
    (error "TIMESTAMP must be a non-negative integer"))
  (let ((id (make-string xid-raw-len 0))
        (counter (prog1 xid-counter
                   (setq xid-counter (logand (1+ xid-counter) #xffffff)))))
    (aset id  0 (logand (ash timestamp -24) #xff))
    (aset id  1 (logand (ash timestamp -16) #xff))
    (aset id  2 (logand (ash timestamp -8) #xff))
    (aset id  3 (logand timestamp #xff))
    (dotimes (i 3)
      (aset id (+ 4 i) (aref xid-machine-id i)))
    (aset id  7 (logand (ash xid-process-id -8) #xff))
    (aset id  8 (logand xid-process-id #xff))
    (aset id  9 (logand (ash counter -16) #xff))
    (aset id 10 (logand (ash counter -8) #xff))
    (aset id 11 (logand counter #xff))
    id))

(defun xid-generate ()
  "Generate a new XID as raw bytes using current time.
Returns a 12-byte string containing the raw XID components."
  (xid-generate-with-timestamp (truncate (float-time))))

(defsubst xid-generate-encoded ()
  "Generate and encode a new XID using current time.
Returns a 20-character base32 encoded string."
  (xid-encode (xid-generate)))

(defun xid--extract-components (raw-id)
  "Extract the components from RAW-ID bytes."
  (let ((timestamp (logior (ash (logand (aref raw-id 0) #xff) 24)
                           (ash (logand (aref raw-id 1) #xff) 16)
                           (ash (logand (aref raw-id 2) #xff) 8)
                           (logand (aref raw-id 3) #xff)))

        (machine-id (list (logand (aref raw-id 4) #xff)
                          (logand (aref raw-id 5) #xff)
                          (logand (aref raw-id 6) #xff)))

        (process-id (logior (ash (logand (aref raw-id 7) #xff) 8)
                            (logand (aref raw-id 8) #xff)))

        (counter (logior (ash (logand (aref raw-id 9) #xff) 16)
                         (ash (logand (aref raw-id 10) #xff) 8)
                         (logand (aref raw-id 11) #xff))))
    (list :timestamp timestamp
          :machine-id machine-id
          :process-id process-id
          :counter counter)))

;;;###autoload
(defun xid-show-components (xid)
  "Decode an XID and print its components.
Argument XID should be a 20-character base32 encoded string."
  (interactive "sXID: ")
  (if (= (length xid) xid-encoded-len)
      (when-let* ((raw-id (xid-decode xid))
                  (components (xid--extract-components raw-id)))
        (message "Timestamp: %s\nMachine ID: 0x%s\nProcess ID: %d\nCounter: %d"
                 (format-time-string "%Y-%m-%dT%H:%M:%S%z" (seconds-to-time (plist-get components :timestamp)))
                 (mapconcat (lambda (byte) (format "%02x" byte))
                            (plist-get components :machine-id)
                            "")
                 (plist-get components :process-id)
                 (plist-get components :counter)))
    (user-error "Invalid XID length: expected 20 characters")))

;;;###autoload
(defun xid-show-components-at-point ()
  "Decode the XID at point and print its components."
  (interactive)
  (let ((xid (thing-at-point 'word t)))
    (if xid
	(xid-show-components xid))))

;;;###autoload
(defun xid-insert ()
  "Insert a new XID at point.
The XID is generated using current time and inserted at the cursor position."
  (interactive)
  (insert (xid-generate-encoded)))

;;;###autoload
(defun xid-copy-to-clipboard ()
  "Generate an XID and copy it to the clipboard.
The XID is generated using the current time."
  (interactive)
  (let ((xid (xid-generate-encoded)))
    (kill-new xid)
    (message "Copied XID to clipboard: %s" xid)))

;; Define a custom menu
;;;###autoload
(easy-menu-define xid-menu nil "XID"
  '("XID"
    ["Insert at point" xid-insert
     :active t
     :help "Insert a new XID at the current cursor position."]
    ["Copy to clipboard" xid-copy-to-clipboard
     :active t
     :help "Generate a new XID and copy it to the clipboard."]
    "---"
    ["Show Components..." xid-show-components
     :active t
     :help "Show the components (timestamp, machine ID, process ID, and counter) of a specified XID."]
    ["Show components at point" xid-show-components-at-point
     :active t
     :help "Show the components (timestamp, machine ID, process ID, and counter) of the XID at the current cursor position."]))

;; Add the menu inside the existing "Tools" menu.
;; Ensure the "Tools" menu exists before adding to it
(let ((tools-menu (lookup-key global-map [menu-bar tools])))
  (when tools-menu
    (define-key-after tools-menu [separator]
      '(menu-item "--"))
    (define-key-after tools-menu [xid]
      (cons "XID" xid-menu))))

(provide 'xid)
;;; xid.el ends here
