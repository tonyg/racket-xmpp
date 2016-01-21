#lang racket/base
;; Error signalling.

(provide (struct-out exn:xmpp)
         (struct-out exn:xmpp:stream)
         xmpp-error)

(struct exn:xmpp exn:fail (stanza) #:transparent)
(struct exn:xmpp:stream exn:xmpp () #:transparent)

(define (xmpp-error fmt
                    #:constructor [constructor exn:xmpp]
                    #:stanza [stanza #f]
                    . args)
  (raise (constructor (apply format fmt args)
                      (current-continuation-marks)
                      stanza)))
