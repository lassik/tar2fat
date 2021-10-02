(define (filter p list)
  (let loop ((newlist '()) (list list))
    (if (null? list) (reverse newlist)
        (loop (if (p (car list)) (cons (car list) newlist) newlist)
              (cdr list)))))

(define (find p list)
  (cond ((null? list) #f)
        ((p (car list)) (car list))
        (else (find p (cdr list)))))

(define (butlast list)
  (if (null? list) (error "butlast: empty list")
      (let loop ((newlist '()) (list list))
        (if (null? (cdr list)) (reverse newlist)
            (loop (cons (car list) newlist) (cdr list))))))

(define (bytevector-concatenate bytevectors)
  (fold (lambda (part whole) (bytevector-append whole part))
        (bytevector) bytevectors))

(define (string-index str char)
  (let loop ((i 0))
    (cond ((= i (string-length str)) #f)
          ((char=? char (string-ref str i)) i)
          (else (loop (+ i 1))))))

(define (string-every? p str)
  (let loop ((i 0))
    (or (= i (string-length str))
        (and (p (string-ref str i))
             (loop (+ i 1))))))

(define (string-prefix? fix str)
  (let ((fixlen (string-length fix))
        (strlen (string-length str)))
    (cond ((< fixlen strlen) (string=? fix (string-copy str 0 fixlen)))
          ((= fixlen strlen) (string=? fix str))
          (else #f))))

(define (string-drop str n)
  (string-copy str n (string-length str)))

(define (string-pad str n char)
  (let ((lack (- n (string-length str))))
    (if (<= lack 0) str (string-append str (make-string lack char)))))

(define (u8 u)
  (bytevector u))

(define (u16l u)
  (bytevector (bitwise-and #xff u)
              (bitwise-and #xff (arithmetic-shift u -8))))

(define (u32l u)
  (bytevector (bitwise-and #xff u)
              (bitwise-and #xff (arithmetic-shift u -8))
              (bitwise-and #xff (arithmetic-shift u -16))
              (bitwise-and #xff (arithmetic-shift u -24))))

;;

(define bytes/sector 512)  ; Do not change.
(define sectors/cluster 8)

(define-record-type fat-directory
  (make-fat-directory* name entries)
  fat-directory?
  (name fat-directory-name)
  (entries fat-directory-entries fat-directory-set-entries!)
  (first-cluster fat-directory-first-cluster
                 fat-directory-set-first-cluster!))

(define-record-type fat-file
  (make-fat-file name contents)
  fat-file?
  (name fat-file-name)
  (contents fat-file-contents)
  (first-cluster fat-file-first-cluster fat-file-set-first-cluster!))

(define-record-type drive
  (make-drive* number root-directory)
  drive?
  (number drive-number)
  (first-sector drive-first-sector)
  (sector-count drive-sector-count drive-set-sector-count!)
  (max-space drive-max-space)
  (free-space drive-free-space)
  (root-directory drive-root-directory))

(define first-drive-letter #\c)
(define last-drive-letter #\f)

(define max-drives
  (+ 1 (- (char->integer last-drive-letter)
          (char->integer first-drive-letter))))

(define drives (make-vector max-drives #f))

(define (get-used-drives)
  (filter drive? (vector->list drives)))

(define (drive-letter drive)
  (string (integer->char (+ (char->integer first-drive-letter)
                            (drive-number drive)))))

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

(define (fat-file-size file)
  (bytevector-length (fat-file-contents file)))

(define (fat-file-clusters file)
  (ceiling (/ (fat-file-size file) (* bytes/sector sectors/cluster))))

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

(define (input-entry entry)
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

(define (input)
  (let loop ()
    (let ((entry (tar-read-entry)))
      (unless (eof-object? entry)
        (input-entry entry)
        (loop)))))

(define (build-file-allocation-table drive)
  (let ((next-free-cluster 0))
    (define (allocate-clusters! n)
      (set! next-free-cluster (+ next-free-cluster n)))
    (let allocate-tree ((directory (drive-root-directory drive)))
      (for-each (lambda (entry)
                  (cond ((fat-directory? entry)
                         (fat-directory-set-first-cluster!
                          entry next-free-cluster)
                         (let ((entries (fat-directory-entries entry)))
                           (allocate-clusters! (* 32 (length entries)))))
                        ((fat-file? entry)
                         (fat-file-set-first-cluster! entry next-free-cluster)
                         (allocate-clusters! (fat-file-clusters entry)))
                        (else
                         (error "What?" entry))))
                (fat-directory-entries directory))
      (for-each allocate-tree
                (filter fat-directory? (fat-directory-entries directory))))))

(define (ascii-alphanumeric? char)
  (or (char<=? #\0 char #\9)
      (char<=? #\A char #\Z)
      (char<=? #\a char #\z)))

(define (valid-fat-filename-stem? str)
  (and (<= (string-length str) 8)
       (string-every? ascii-alphanumeric? str)))

(define (valid-fat-filename-extension? str)
  (and (<= (string-length str) 3)
       (string-every? ascii-alphanumeric? str)))

(define (encode-8.3-filename str)
  (let* ((dot (string-index str #\.))
         (stem (if dot (string-copy str 0 dot) str))
         (ext (if dot (string-copy str (+ dot 1) (string-length str)) "")))
    (unless (valid-fat-filename-stem? stem)
      (error "stem" stem))
    (unless (valid-fat-filename-extension? ext)
      (error "ext" dot ext stem))
    (if (and (valid-fat-filename-stem? stem)
             (valid-fat-filename-extension? ext))
        (string->utf8 (string-append (string-pad stem 8 #\space)
                                     (string-pad ext 3 #\space)))
        (error "Bad 8.3 filename" str))))

(define (encode-directory-entry entry)
  (let ((attributes (bitwise-ior #x20 (if (fat-directory? entry) #x10 0))))
    (bytevector-append
     (encode-8.3-filename (fat-directory-entry-name entry))
     (u8 attributes)
     (u8 0)  ; misc non-portable info
     (u8 0)  ; misc non-portable info
     (u16l 0)  ; create time-of-day
     (u16l 0)  ; create date
     (u16l 0)  ; access date
     (u16l 0)  ; misc non-portable info
     (u16l 0)  ; modify time-of-day
     (u16l 0)  ; modify date
     (u16l (if (fat-file? entry) (fat-file-first-cluster entry) 0))
     (u32l (if (fat-file? entry) (fat-file-size entry) 0)))))

(define (write-tree directory)
  (write-bytevector
   (bytevector-concatenate
    (map encode-directory-entry
         (fat-directory-entries directory))))
  (for-each write-tree
            (filter fat-directory? (fat-directory-entries directory))))

(define (write-drive drive)
  (write-tree (drive-root-directory drive)))

(define (display-drive drive)
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
  (emit 0 (drive-letter drive))
  (dump-directory-entries 1 (drive-root-directory drive)))

(define (main)
  (input)
  (for-each build-file-allocation-table (get-used-drives))
  (for-each display-drive (get-used-drives))
  (for-each write-drive (get-used-drives)))

(main)
