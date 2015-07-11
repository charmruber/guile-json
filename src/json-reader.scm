(define-module (json reader))
(export
    json:read-string
    json:read-number
    json:read-bool
    json:read-null
    json:null
    json:read-list
    json:read-object
    json:read-value
)
(use-modules (ice-9 rdelim))

; Unescapes \ excaped characters
(define (json:backslash fd)
    (let ((c (read-char fd)))
        (case c
            ((#\n) "\n")
            ((#\t) "\t")
            ((#\f) "\f")
            ((#\r) "\r")
            ((#\b) "\b")
            ((#\\) "\\")
            ((#\") "\"")
            ((#\/) "/")
            ((#\u)
                (let ((num (string->number (string (read-char fd) (read-char fd) (read-char fd) (read-char fd)) 16)))
                    (if (> num 255)
                        (set! num 255)) ;FIXME
                    (string (integer->char num ))
                )
            )
        )
    )
)
; This func is for representing JSON's null
(define (json:null) '())
(define (json:read-string fd)
    (let getstring ((current (read-delimited "\\\"" fd 'split)))
        (case (cdr current)
            ((#\")
                (car current)
            )
            ((#\\)
                (let ((newc (json:backslash fd)) (temp (read-delimited "\\\"" fd 'split)))
                    (list-set! temp 0 (string-append (car current) newc (car temp)))
                    (getstring temp)
                )
            )
            (else
                (error "Error parsing JSON")
            )
        )
    )
)
; Helper function for getting the key-value pair from a map/object/whaterver you wanna call it
