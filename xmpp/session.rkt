#lang racket/base
;; XMPP client session establishment and stanza I/O.

(require racket/set)
(require racket/match)
(require racket/tcp)
(require openssl)
(require net/base64)

(require "jid.rkt")
(require "stanza.rkt")
(require "error.rkt")

(provide (struct-out xmpp-session)
	 xmpp-connect
         xmpp-open-session
	 xmpp-disconnect
	 xmpp-flush

         xmpp-send
         xmpp-receive-evt
         xmpp-receive

         xmpp-send-iq
         xmpp-send-iq/get
         xmpp-send-iq/set
         xmpp-send-iq/result
         xmpp-send-iq-reply

         xmpp-send-presence
         xmpp-send-message
         )

(struct xmpp-session (input
                      output
                      encrypted?
                      features
                      jid)
  #:transparent
  #:property prop:evt 0)

(define (session-exn-closer session)
  (lambda (e)
    (xmpp-disconnect session)
    (raise e)))

(define (lookup-srv host)
  (define (return-defaults)
    (values #f #f))
  (with-handlers [(exn:fail:contract? (lambda (e) (return-defaults)))]
    (local-require (only-in net/dns dns-find-nameserver))
    (struct srv-rr (priority weight port target) #:prefab)
    (define dynamic:dns-get-srv (dynamic-require 'net/dns 'dns-get-srv))
    ;; ^ available in racket >= 6.3.0.7
    (define rrs (dynamic:dns-get-srv (dns-find-nameserver) host "xmpp-client"))
    ;; TODO: fix dns-get-srv to return them in a sensible order
    (match rrs
      ['() (return-defaults)]
      [(cons (srv-rr _ _ port target) _) (values target port)])))

(define (xmpp-connect j
                      password
                      #:use-srv? [use-srv? #t]
                      #:use-tls? [use-tls? #t]
                      #:hostname [hostname #f]
                      #:ssl-port-number [ssl-port-number 5223]
                      #:port-number [port-number 5222])
  (cond [use-srv?
         (match/values (lookup-srv (jid-host j))
           [(#f #f) (xmpp-open-ssl-connection hostname
                                              ssl-port-number
                                              port-number
                                              use-tls?
                                              j
                                              password)]
           [(h p) (xmpp-open-tcp-connection h p use-tls? j password)])]
        [else
         (xmpp-open-ssl-connection hostname
                                   ssl-port-number
                                   port-number
                                   use-tls?
                                   j
                                   password)]))

(define (xmpp-open-ssl-connection hostname ssl-port-number port-number use-tls? j password)
  (if (and use-tls? ssl-port-number)
      (with-handlers
        [(exn:fail:network? (lambda (e)
                              (xmpp-open-tcp-connection hostname
                                                        port-number
                                                        use-tls?
                                                        j
                                                        password)))]
        (define-values (i o) (ssl-connect (or hostname (jid-host j)) ssl-port-number))
        (xmpp-open-session i o #t use-tls? j password))
      (xmpp-open-tcp-connection hostname port-number use-tls? j password)))

(define (xmpp-open-tcp-connection h p use-tls? j password)
  (define-values (i o) (tcp-connect (or h (jid-host j)) p))
  (xmpp-open-session i o #f use-tls? j password))

(define (restart-stream i o j)
  (write-stream-header o (jid-host j))
  (read-stream-header i)
  (match-define `(stream:features () ,fs ...) (read-stanza i))
  fs)

(define (xmpp-open-session i o encrypted? use-tls? j password)
  (let ((features (restart-stream i o j)))
    (if (and (not encrypted?)
             use-tls?
             (memf (match-lambda [`(starttls ((xmlns "urn:ietf:params:xml:ns:xmpp-tls")) ,_ ...) #t]
                                 [_ #f])
                   features))
        (let-values (((si so) (negotiate-tls i o)))
          (define pi si)
          ;; (define-values (pi po) (make-pipe))
          ;; (thread (lambda ()
          ;;           (let loop ()
          ;;             (match (read-char si)
          ;;               [(? eof-object? e) (close-output-port po)]
          ;;               [c (write-char c) (flush-output) (write-char c po) (loop)]))))
          (authenticate pi so #t (restart-stream pi so j) j password))
        (authenticate i o encrypted? features j password))))

(define (negotiate-tls i o)
  (write-stanza o '(starttls ((xmlns "urn:ietf:params:xml:ns:xmpp-tls"))) #:flush? #t)
  (match (read-stanza i)
    [`(failure ,_ ...) (xmpp-error "STARTTLS failure")]
    [`(proceed ,_ ...) (ports->ssl-ports i o #:mode 'connect #:close-original? #t)]))

(define (authenticate i o encrypted? features j password)
  (when (not encrypted?)
    (xmpp-error "XMPP connection not encrypted; no secure authentication mechanism implemented"))
  (write-stanza o
                `(auth ((xmlns "urn:ietf:params:xml:ns:xmpp-sasl")
                        (mechanism "PLAIN"))
                       ,(bytes->string/utf-8
                         (base64-encode (bytes-append (bytes 0)
                                                      (string->bytes/utf-8 (jid-user j))
                                                      (bytes 0)
                                                      (string->bytes/utf-8 password))
                                        #"")))
                #:flush? #t)
  (define response (read-stanza i))
  (match response
    [`(failure ,_ ...) (xmpp-error "Authentication failure" #:stanza response)]
    [`(success ((xmlns "urn:ietf:params:xml:ns:xmpp-sasl")))
     (define s (xmpp-session i o encrypted? (restart-stream i o j) j))
     (bind s)]))

(define (bind session)
  (when (memf (match-lambda [`(bind ((xmlns "urn:ietf:params:xml:ns:xmpp-bind")) ,_ ...) #t]
                            [_ #f])
              (xmpp-session-features session))
    (xmpp-send-iq/set session
                      `(bind ((xmlns "urn:ietf:params:xml:ns:xmpp-bind"))
                             ,@(let ((resource (jid-resource (xmpp-session-jid session))))
                                 (maybe-elements resource `(resource ,resource)))))
    (match (xmpp-receive session)
      [`(iq ,_ (bind ,_ (jid ,_ ,jidstr)))
       (set! session (struct-copy xmpp-session session [jid (string->jid jidstr)]))]))
  (when (member '(session ((xmlns "urn:ietf:params:xml:ns:xmpp-session")))
                (xmpp-session-features session))
    (xmpp-send-iq/set session `(session ((xmlns "urn:ietf:params:xml:ns:xmpp-session"))))
    (match (xmpp-receive session)
      [`(iq . ,_) (void)]))
  session)

(define (xmpp-disconnect session)
  (xmpp-flush session)
  (close-input-port (xmpp-session-input session))
  (close-output-port (xmpp-session-output session)))

(define (xmpp-flush session)
  (flush-output (xmpp-session-output session)))

(define (xmpp-send session stanza #:flush? [flush? #t])
  (write-stanza (xmpp-session-output session) stanza #:flush? flush?))

(define (xmpp-receive-evt session #:return-errors? [return-errors? #f])
  (xmpp-flush session)
  (handle-evt (xmpp-session-input session)
              (lambda (_)
                (maybe-raise-error return-errors?
                                   (read-stanza (xmpp-session-input session))))))

(define (xmpp-receive session
                      #:timeout [timeout #f]
                      #:return-errors? [return-errors? #f])
  (sync/timeout timeout (xmpp-receive-evt session #:return-errors? return-errors?)))

(define (maybe-raise-error return-errors? stanza)
  (if return-errors?
      stanza
      (match stanza
        [`(stream:error ,_ ...) (xmpp-error "Error from server: ~v" stanza
                                            #:constructor exn:xmpp:stream
                                            #:stanza stanza)]
        [_ stanza])))

(define (xmpp-send-iq* session to from id type body-elements)
  (when (not id) (set! id (gensym 'iq)))
  (xmpp-send session
             `(iq ((id ,(if (symbol? id) (symbol->string id) id))
                   ,@(maybe-elements to `(to ,(jid->string to)))
                   ,@(maybe-elements from `(from ,(jid->string from)))
                   (type ,type))
                  ,@body-elements))
  id)

(define (xmpp-send-iq session
                      #:id [id #f]
                      #:to [to #f]
                      #:from [from #f]
                      #:type [type "set"]
                      . body-elements)
  (xmpp-send-iq* session to from id type body-elements))

(define (xmpp-send-iq/get session #:id [id #f] #:to [to #f] #:from [from #f] . body-elements)
  (xmpp-send-iq* session to from id "get" body-elements))

(define (xmpp-send-iq/set session #:id [id #f] #:to [to #f] #:from [from #f] . body-elements)
  (xmpp-send-iq* session to from id "set" body-elements))

(define (xmpp-send-iq/result session #:id [id #f] #:to [to #f] #:from [from #f] . body-elements)
  (xmpp-send-iq* session to from id "result" body-elements))

(define (xmpp-send-iq-reply session req-stanza . body-elements)
  (match-define `(iq ,req-attrs . ,_) req-stanza)
  (match-define `(to ,req-target) (assq 'to req-attrs))
  (match-define `(from ,req-source) (assq 'from req-attrs))
  (match-define `(id ,req-id) (assq 'id req-attrs))
  (xmpp-send-iq* session
                 (string->jid req-source)
                 (string->jid req-target)
                 req-id
                 "result"
                 body-elements))

(define (xmpp-send-presence session
                            #:to [to #f]
                            #:from [from #f]
                            #:type [type #f]
                            #:show [show #f]
                            #:status [status #f]
                            . body-elements)
  (xmpp-send session
             `(presence (,@(maybe-elements to `(to ,(jid->string to)))
                         ,@(maybe-elements from `(from ,(jid->string from)))
                         ,@(maybe-elements type `(type ,type)))
                        ,@(maybe-elements show `(show ,show))
                        ,@(maybe-elements status `(status ,status))
                        ,@body-elements)))

(define (xmpp-send-message session
                           #:to [to #f]
                           #:from [from #f]
                           #:type [type "chat"]
                           . body-elements)
  (xmpp-send session
             `(message (,@(maybe-elements to `(to ,(jid->string to)))
                        ,@(maybe-elements from `(from ,(jid->string from)))
                        ,@(maybe-elements type `(type ,type)))
                       ,@body-elements)))
