(define (find p list)
  (cond ((null? list) #f)
        ((p (car list)) (car list))
        (else (find p (cdr list)))))

(define (butlast list)
  (if (null? list) (error "butlast: empty list")
      (let loop ((newlist '()) (list list))
        (if (null? (cdr list)) (reverse newlist)
            (loop (cons (car list) newlist) (cdr list))))))

(define (string-prefix? fix str)
  (let ((fixlen (string-length fix))
        (strlen (string-length str)))
    (cond ((< fixlen strlen) (string=? fix (string-copy str 0 fixlen)))
          ((= fixlen strlen) (string=? fix str))
          (else #f))))

(define (string-drop str n)
  (string-copy str n (string-length str)))

;;

(define-record-type fat-directory
  (make-fat-directory* name entries)
  fat-directory?
  (name fat-directory-name)
  (entries fat-directory-entries fat-directory-set-entries!))

(define-record-type fat-file
  (make-fat-file name contents)
  fat-file?
  (name fat-file-name)
  (contents fat-file-contents))

(define-record-type drive
  (make-drive* number root-directory)
  drive?
  (number drive-number)
  (root-directory drive-root-directory))

(define first-drive-letter #\C)
(define last-drive-letter #\F)

(define max-drives
  (- (char->integer last-drive-letter) (char->integer first-drive-letter)))

(define drives (make-vector max-drives #f))

(define (drive-letter drive)
  (integer->char (+ (char->integer first-drive-letter) (drive-number drive))))

(define (drive-by-letter letter)
  (let ((number (- (char->integer letter) first-drive-letter)))
    (if (and (>= number 0) (< number (vector-length drives)))
        (vector-ref drives number)
        (error "No such drive"))))

(define (parse-drive-number drive-letter-string)
  (define (bad) (error "Bad drive letter" drive-letter-string))
  (unless (= 1 (string-length drive-letter-string)) (bad))
  (let ((char (string-ref drive-letter-string 0)))
    (unless (char<=? #\a char #\z) (bad))
    (let ((number (- (char->integer char) (char->integer #\a))))
      (if (< number max-drives) number
          (error "Too many drives" drive-letter-string)))))

(define (get-drive drive-letter-string)
  (let ((number (parse-drive-number drive-letter-string)))
    (vector-ref drives number)))

(define (get-or-create-drive drive-letter-string)
  (let ((number (parse-drive-number drive-letter-string)))
    (or (vector-ref drives number)
        (let ((drive (make-drive number)))
          (vector-set! drives number drive)
          drive))))

(define (make-fat-directory name)
  (make-fat-directory* name '()))

(define (make-drive number)
  (make-drive* number (make-fat-directory* #f '())))

(define (fat-directory-entry-name entry)
  (cond ((fat-directory? entry)
         (fat-directory-name entry))
        ((fat-file? entry)
         (fat-file-name entry))
        (else
         (error "What?" entry))))

(define (fat-directory-insert-entry! directory entry)
  (let ((name (fat-directory-entry-name entry))
        (head (cons #f (fat-directory-entries directory))))
    (let loop ((tail head))
      (if (or (null? (cdr tail))
              (string-ci<? name (fat-directory-entry-name (cadr tail))))
          (set-cdr! tail (cons entry (cdr tail)))
          (loop (cdr tail))))
    (fat-directory-set-entries! directory (cdr head))))

(define (create-directory directory path)
  (if (null? path) directory
      (let* ((name (car path))
             (path (cdr path))
             (subdir (make-fat-directory name)))
        (fat-directory-insert-entry! directory subdir)
        (create-directory subdir path))))

(define (drive-get-or-create-directory drive path)
  (let loop ((directory (drive-root-directory drive)) (path path))
    (if (null? path) directory
        (let* ((name (car path))
               (entry (find (lambda (entry)
                              (equal? name (fat-directory-entry-name entry)))
                            (fat-directory-entries directory))))
          (cond ((not entry)
                 (create-directory directory path))
                ((fat-directory? entry)
                 (loop entry (cdr path)))
                (else
                 (error "Can't decide if entry is file or directory")))))))

(define (tar-read-entry)

  (define (bytevector-every? match? bytes)
    (let loop ((i 0))
      (or (= i (bytevector-length bytes))
          (and (match? (bytevector-u8-ref bytes i))
               (loop (+ i 1))))))

  (define (read-exactly-n-bytes n)
    (let ((bytes (read-bytevector n)))
      (let ((bytes (if (eof-object? bytes) (bytevector) bytes)))
        (if (< (bytevector-length bytes) n)
            (error "Short read")
            bytes))))

  (let ((header (read-bytevector 512)))

    (define (tar-string-copy offset len)
      (utf8->string
       (bytevector-copy
        header offset
        (let loop ((end (+ offset len)))
          (let ((i (- end 1)))
            (if (< i offset) (error "tar string")
                (if (zero? (bytevector-u8-ref header i))
                    (loop i)
                    end)))))))

    (define (tar-octal-ref offset len)
      (let loop ((offset offset) (len len) (value 0))
        (if (<= len 0) value
            (let ((dig0 (char->integer #\0))
                  (dig7 (char->integer #\7))
                  (byte (bytevector-u8-ref header offset)))
              (loop (+ offset 1) (- len 1)
                    (if (<= dig0 byte dig7)
                        (let ((digit (- byte dig0)))
                          (+ digit (* value 8)))
                        value))))))

    (cond ((eof-object? header)
           (eof-object))
          ((bytevector-every? zero? header)
           (eof-object))
          (else
           (unless (= 512 (bytevector-length header))
             (error "Short read"))
           (let* ((nbyte (tar-octal-ref 124 12))
                  (nnull (truncate-remainder
                          (- 512 (truncate-remainder nbyte 512))
                          512))
                  (bytes (read-exactly-n-bytes nbyte)))
             (read-exactly-n-bytes nnull)
             (list (cons 'path (tar-string-copy 0 100))
                   (cons 'type
                         (let ((type
                                (integer->char
                                 (bytevector-u8-ref header 156))))
                           (case type
                             ((#\0 #\nul) 'file)
                             ((#\5) 'directory)
                             (else (error "Weird tar entry type" type)))))
                   (cons 'contents bytes)))))))

(define (tar-split-path whole)
  (let ((parts
         (let loop ((a 0) (b 0) (parts '()))
           (cond ((= a b (string-length whole))
                  (reverse parts))
                 ((= b (string-length whole))
                  (loop b b (cons (string-copy whole a b) parts)))
                 ((char=? #\/ (string-ref whole b))
                  (loop (+ b 1) (+ b 1) (cons (string-copy whole a b) parts)))
                 (else
                  (loop a (+ b 1) parts))))))
    parts))

(define (dump-drive letter)
  (define (emit nest str)
    (write-string (make-string (* 2 nest) #\space))
    (write-string str)
    (newline))
  (define (dump-directory-entries nest directory)
    (for-each (lambda (entry)
                (emit nest (fat-directory-entry-name entry))
                (when (fat-directory? entry)
                  (dump-directory-entries (+ nest 1) entry)))
              (fat-directory-entries directory)))
  (let ((drive (get-drive letter)))
    (when drive
      (emit 0 letter)
      (dump-directory-entries 1 (drive-root-directory drive)))))

(define (handle-entry entry)
  (let ((path (tar-split-path (cdr (assq 'path entry)))))
    (when (null? path)
      (error "blank path in tar file"))
    (let ((first (car path)))
      (cond ((equal? "boot" first)
             (error "Boot"))
            ((string-prefix? "drive_" first)
             (let ((drive (get-or-create-drive
                           (string-drop first (string-length "drive_"))))
                   (path (cdr path)))
               (case (cdr (assq 'type entry))
                 ((directory)
                  (drive-get-or-create-directory drive path))
                 ((file)
                  (let* ((name (last path))
                         (path (butlast path)))
                    (fat-directory-insert-entry!
                     (drive-get-or-create-directory drive path)
                     (make-fat-file name
                                    (cdr (assq 'contents entry)))))))))))))

(define (main)
  (let loop ()
    (let ((entry (tar-read-entry)))
      (unless (eof-object? entry)
        (handle-entry entry)
        (loop)))))

(main)
(dump-drive "c")
