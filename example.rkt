#lang racket
;; Trivial CLI xmpp client.

(require xmpp)

(define-values (self-jid-str password)
  (let ((credentials-file "racket-xmpp-credentials.rktd"))
    (if (file-exists? credentials-file)
        (with-input-from-file credentials-file
          (lambda ()
            (values (read-line)
                    (read-line))))
        (let ((prompt (lambda (p)
                        (printf "~a: " p)
                        (flush-output)
                        (read-line))))
          (values (prompt "JID")
                  (prompt "Password"))))))

(define self-jid (string->jid self-jid-str))

(define s (xmpp-connect self-jid password))

(xmpp-send-presence s)
(printf "Connected.\n")

(let loop ((target #f))
  (sync (handle-evt (read-line-evt (current-input-port) 'any)
                    (lambda (line)
                      (match line
                        [(pregexp "^/to (.*)$" (list _ to))
                         (match (string->jid to #f)
                           [#f (loop target)]
                           [new-target
                            (printf "Now sending to ~a.\n" new-target)
                            (loop new-target)])]
                        [(pregexp "^/status$" (list _))
                         (xmpp-send-presence s)
                         (loop target)]
                        [(pregexp "^/status (.*)$" (list _ new-status))
                         (xmpp-send-presence s #:status new-status)
                         (loop target)]
                        [(pregexp "^/accept (.*)$" (list _ who))
                         (match (string->jid who #f)
                           [#f (void)]
                           [j (xmpp-send-presence s #:type "subscribed" #:to j)])
                         (loop target)]
                        [(or (? eof-object?) (pregexp "^/quit$" (list _)))
                         (void)]
                        [(pregexp "^ *$" (list _))
                         (loop target)]
                        [_
                         (if target
                             (xmpp-send-message s #:to target `(body ,line))
                             (printf "Target not set. Try '/to some@jid.example'.\n"))
                         (loop target)])))
        (handle-evt (xmpp-receive-evt s)
                    (lambda (stanza)
                      (match stanza
                        [`(iq ,_ (ping ((xmlns "urn:xmpp:ping"))))
                         (printf "(got ping)\n")
                         (xmpp-send-iq-reply s stanza)]
                        [`(message ,attrs ,@elements)
                         (for [(e elements)]
                           (match e
                             [`(body () ,body)
                              (printf "~a: ~a\n"
                                      (cadr (assq 'from attrs))
                                      body)]
                             [_ (void)]))]
                        [_
                         (pretty-print stanza)])
                      (loop target)))))

(xmpp-send-presence s #:type "unavailable")
(xmpp-disconnect s)
