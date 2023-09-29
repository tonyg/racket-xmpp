#lang racket/base
;; XMPP Stream and stanza I/O.

(provide read-stream-header
         read-stanza
         write-stream-header
         write-stanza
         maybe-elements)

(require racket/match)
(require xml)

(require "error.rkt")

(define-logger xmpp)

(define (read-stream-header p)
  (define header
    (and (skip-whitespace p)
         (check-drop p #"<?")
         (capture-until p 63) ;; #\?
         (check-drop p #">")
         (skip-whitespace p)
         (check-drop p #"<stream:stream")
         (let ((stream-header-contents (capture-until p 62))) ;; #\>
           (and stream-header-contents
                (xml->xexpr (read-xml/element
                             (open-input-bytes (bytes-append #"<stream:stream"
                                                             stream-header-contents
                                                             #"></stream:stream>"))))))))
  (log-xmpp-debug "Stream header: ~v" header)
  header)

(define (skip-whitespace p)
  (match (peek-byte p)
    [(? eof-object? e) #f]
    [n #:when (<= n 32)
       (read-byte p)
       (skip-whitespace p)]
    [_ #t]))

(define (check-drop p bs)
  (for/and [(i (in-range (bytes-length bs)))]
    (match (read-byte p)
      [(== (bytes-ref bs i)) #t]
      [_ #f])))

(define (capture-until p terminator)
  (let loop ((acc-rev '()))
    (match (read-byte p)
      [(? eof-object?) #f]
      [(== terminator) (list->bytes (reverse acc-rev))]
      [b (loop (cons b acc-rev))])))

(define (read-stanza p)
  (define stanza
    (if (eof-object? (peek-byte p))
        eof
        (let ((stanza (xml->xexpr (document-element (read-xml/document p)))))
          (match stanza
            [`(stream:stream ,_ ,elt) elt] ;; TODO: proper xmlns check
            [`(stream:stream ,_ ...) ;; TODO: proper xmlns check
             (xmpp-error "Invalid restarted stream" #:stanza stanza)]
            [_ stanza]))))
  (log-xmpp-debug "Incoming stanza: ~v" stanza)
  stanza)

(define (write-stream-header p host)
  (log-xmpp-debug "Writing stream header for host ~v" host)
  (display "<?xml version=\"1.0\"?>" p)
  (match-define (list _ open-tag)
    (regexp-match #px#"(.*)</stream:stream>"
                  (xexpr->string `(stream:stream
                                   ((xmlns:stream "http://etherx.jabber.org/streams")
                                    (to ,host)
                                    (version "1.0")
                                    (xmlns "jabber:client"))))))
  (display open-tag p)
  (flush-output p))

(define (write-stanza p stanza #:flush? [flush? #t])
  (log-xmpp-debug "Outgoing stanza: ~v" stanza)
  (parameterize ((empty-tag-shorthand 'always))
    (write-xexpr stanza p))
  (when flush? (flush-output p)))

(define-syntax-rule (maybe-elements test elt ...)
  (if test (list elt ...) '()))
