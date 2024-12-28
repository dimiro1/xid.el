;;; xid-test.el --- Tests for xid.el -*- lexical-binding: t -*-

(require 'ert)
(require 'xid)

(ert-deftest xid-test-padding ()
  "Test that Base32 encoded strings are padded correctly."
  (dotimes (_ 100)
    (let* ((raw-id (xid-generate))
           (encoded (xid-encode raw-id)))
      (should (= (length encoded) xid-encoded-len))
      (should (string-match-p "^[0-9a-v]+$" encoded)))))

(ert-deftest xid-test-known-encoded-string ()
  "Test that a known raw XID encodes to the expected Base32 string."
  (let* ((raw-id (string #x4d #x88 #xe1 #x5b #x60 #xf4 #x86 #xe4 #x28 #x41 #x2d #xc9))
         (expected "9m4e2mr0ui3e8a215n4g")
         (encoded (xid-encode raw-id)))
    (should (string= encoded expected))))

(ert-deftest xid-test-invalid-inputs ()
  "Test invalid inputs for decoding."
  (should-error (xid-decode "invalid"))         ;; Invalid characters
  (should-error (xid-decode "9m4e2mr0ui3e8a")) ;; Too short
  (should-error (xid-decode "9m4e2mr0ui3e8a215n4ggggg")) ;; Too long
  (should-error (xid-decode "abcde12345@#$%"))); Invalid symbols

(ert-deftest xid-test-counter-increment ()
  "Test that the counter increments between generated XIDs."
  (let* ((id1 (xid-generate))
         (id2 (xid-generate))
         (counter1 (+ (* (aref id1 9) 65536)
                      (* (aref id1 10) 256)
                      (aref id1 11)))
         (counter2 (+ (* (aref id2 9) 65536)
                      (* (aref id2 10) 256)
                      (aref id2 11))))
    (should (= (1+ counter1) counter2))))

(ert-deftest xid-test-uniqueness ()
  "Test that generated XIDs are unique."
  (let ((xids (make-hash-table :test 'equal)))
    (dotimes (_ 1000)
      (let ((xid (xid-generate-encoded)))
        (should-not (gethash xid xids))
        (puthash xid t xids)))))

(ert-deftest xid-test-encoding-decoding ()
  "Test encoding and decoding of known XIDs."
  (let* ((raw-id (string #x4d #x88 #xe1 #x5b #x60 #xf4 #x86 #xe4 #x28 #x41 #x2d #xc9))
         (expected "9m4e2mr0ui3e8a215n4g")
         (encoded (xid-encode raw-id))
         (decoded (xid-decode encoded)))
    (message encoded)
    (should (string= encoded expected))
    (should (string= expected (xid-encode decoded)))))

(provide 'xid-test)
;;; xid-test.el ends here