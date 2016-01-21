#lang racket/base
;; XMPP JID structure and utilities.

(provide (struct-out jid)
         string->jid
         jid->string)

(require racket/match)

(struct jid (user
             host
             resource) ;; may be #f
  #:prefab)

(define (string->jid s)
  (match s
    [(regexp #px"^(([^@]+)@)?([^/]+)(/(.*))?$" (list _ _ u h _ r)) (jid u h r)]
    [_ (error 'string->jid "Ill-formed jid: ~v" s)]))

(define (jid->string j)
  (match-define (jid u h r) j)
  (string-append (if u (string-append u "@") "")
                 h
                 (if r (string-append "/" r) "")))

(module+ test
  (require rackunit)
  (check-equal? (string->jid "a@b.c") (jid "a" "b.c" #f))
  (check-equal? (string->jid "b.c") (jid #f "b.c" #f))
  (check-exn #px"Ill-formed jid" (lambda () (string->jid "")))
  (check-equal? (string->jid "a@a@b.c") (jid "a" "a@b.c" #f))
  (check-equal? (string->jid "b.c/d/e") (jid #f "b.c" "d/e"))
  (check-equal? (string->jid "a@b.c/d") (jid "a" "b.c" "d"))
  (check-equal? (string->jid "b.c/d") (jid #f "b.c" "d"))
  (check-equal? (string->jid "tonyg@example.com/myresource") (jid "tonyg" "example.com" "myresource")))
