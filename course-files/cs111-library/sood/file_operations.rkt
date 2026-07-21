#lang racket/base

  (provide path? path-string? path->string string->path build-path path-has-extension?
           file-exists? file-or-directory-modify-seconds file-size copy-file! rename-file-or-directory!
           delete-file! delete-directory!
           directory-exists? make-directory!
           directory-files directory-subdirectories)
  (provide path-directory path-filename)
  (require racket/contract/base
           mzlib/pconvert
           (for-syntax racket/base syntax/parse/pre racket/sequence))

  (let ([ph (current-print-convert-hook)])
    (current-print-convert-hook
     (λ (val basic sub)
       (cond [(path? val)
              `(string->path ,(path->string val))]
             [else (ph val basic sub)]))))

  ;; Check if maybe-path is a path-string?. If not, raise argument error
  ;; using the given name, bad-pos and v ... arguments with a predefined error message.
  (define (check-is-path maybe-path name bad-pos . v...)
    (unless (path-string? maybe-path)
      (define additional-type-information
        (cond [(boolean? maybe-path) ", not true or false"]
              [(number? maybe-path) ", not a number"]
              [((listof path-string?) maybe-path) ", not a list of paths"]
              [(list? maybe-path) ", not a list"]
              [else ""]))
      (apply raise-argument-error
        name
        (string-append
         (symbol->string name)
         " takes a path"
         additional-type-information)
        bad-pos
        v...)))

  (define (check-directory-exist name path)
    (unless (directory-exists? path)
      (raise-argument-error name "a path to a directory" path)))

  (define (safe-path! name p pos)
    (unless (and (relative-path? p) (not (memq 'up (explode-path p))))
      (raise-argument-error name "a relative path with no `..'s" pos p)))

  ;; A wrapped call to directory-list with #:build? #t, where the `filesystem errno'
  ;; error messages (if any) are renamed to point to `name' instead of `directory-list'
  (define (directory-list-with-renamed-exn name path)
    (with-handlers ([exn:fail:filesystem:errno?
                     (lambda (e)
                       (raise (exn:fail:filesystem:errno
                               (regexp-replace
                                "^directory-list"
                                (exn-message e)
                                (symbol->string name))
                               (exn-continuation-marks e)
                               (exn:fail:filesystem:errno-errno e))))])
      (directory-list path #:build? #t)))

  ;; wrappers to make things safe

  (define (copy-file! src dest [exists-ok? #f])
    (safe-path! 'copy-file! src 0)
    (safe-path! 'dest-file! dest 0)
    (copy-file src dest exists-ok?))

  (define (rename-file-or-directory! old new [exists-ok? #f])
    (safe-path! 'rename-file-or-directory! old 0)
    (safe-path! 'rename-file-or-directory! new 0)
    (rename-file-or-directory old new exists-ok?))

  (define (delete-file! p)
    (safe-path! 'delete-file! p 0)
    (delete-file p))

  (define (delete-directory! p)
    (safe-path! delete-directory! p 0)
    (delete-directory p))

  (define (make-directory! p)
    (safe-path! 'make-directory! p 0)
    (make-directory p))

  ;; Special stuff defined here


  (define (path-directory path)
    (check-is-path path 'path-directory 0 path)
    (define-values (directory filename must-be-dir?) (split-path path))
    directory)

  (define (path-filename path)
    (check-is-path path 'path-filename 0 path)
    (define-values (directory filename must-be-dir?) (split-path path))
    filename)

  (define (directory-subdirectories path)
    (check-is-path path 'directory-subdirectories 0 path)
    (safe-path! 'directory-subdirectories path 0)
    (check-directory-exist 'directory-subdirectories path)
    (filter directory-exists?
            (directory-list-with-renamed-exn 'directory-subdirectories path)))

  (define (directory-files path)
    (check-is-path path 'directory-files 0 path)
    (safe-path! 'directory-files path 0)
    (check-directory-exist 'directory-files path)
    (filter file-exists?
            (directory-list-with-renamed-exn 'directory-files path)))

  (define (path-has-extension? path extension)
    (check-is-path path 'path-has-extension? 0 path extension)
    (unless (string? extension)
      (raise-argument-error 'path-has-extension? "string?" 1 path extension))
    (define string (if (string? path)
                       path
                       (path->string path)))
    (define l (string-length string))
    (define ext-length (string-length extension))
    (define maybe-extension
      (if (< ext-length l)
          (substring string (- l ext-length) l)
          ""))
    (string=? extension maybe-extension))
