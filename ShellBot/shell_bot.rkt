#lang racket
 
;;; ____________________________________________________________________________________________________________________________________
;;;    Global Variable Definitions:

(define OWNER_IDS '(""))
(define BOT_TOKEN "")
(define BOT_PREFIX ">")
 
;;; ____________________________________________________________________________________________________________________________________

(require racket-cord json racket/system)
(define bot (make-client BOT_TOKEN #:auto-shard #t #:intents (list intent-guilds intent-guild-messages)))

;;; ____________________________________________________________________________________________________________________________________
;;;    Helper Procedures:

(define (isowner? id) (if (member id OWNER_IDS) #t #f))
(define (content payload) (hash-ref  payload 'content))
(define (channel payload) (hash-ref payload 'channel_id))
(define (author_id payload) (hash-ref (hash-ref payload 'author) 'id))

(define (iswindows?) (symbol=? (system-type 'os) 'windows)) ;; Yes ik windows bad, but it's better than checking for unix and macos
(define shell (if (iswindows?) "powershell.exe" "/usr/bin/bash"))
(define (execute_shell cmd) (string-trim  
        (if (iswindows?) 
            (with-output-to-string (lambda () (system* (find-executable-path "powershell.exe") cmd)))
            (with-output-to-string (lambda () (system cmd)))
        )
    )
)

(define (make_embed #:title [title ""] #:description [description ""] #:color [color 0]) 
    (string->jsexpr 
        (format "{\"title\":\"~a\",\"color\":~a, \"description\":\"~a\"}" title color description)
    )
)

;;; ____________________________________________________________________________________________________________________________________
;;;    Commands:

(define (shell_cmd client payload params)
    (let (
        [cmd      (string-join (cdr params))]
        [send_out (lambda (output) (http:create-message client (channel payload) output))]
    )
        (if (non-empty-string? cmd)
            (with-handlers ([exn:fail? 
                    (lambda (e) (send_out (format "**An error occured:**\n~a" e))) 
                ])
                (let ([output (execute_shell cmd)])
                    (if (non-empty-string? output) 
                        (send_out (format "~a" output))
                        (send_out "**Warning:** The shell did not respond with any output. Did the command write to the right output?")
                    )
                )
            )
            (send_out (format "Please supply `~a` code to be executed." shell))
        )
    )
)

;;; ____________________________________________________________________________________________________________________________________
;;;    Event Handlers:

;;; Command handler:
(on-event
    'raw-message-create bot 
    (lambda (ws-client client payload)
        (let ([msg (content payload)])
            (when (string-prefix? msg BOT_PREFIX)
                (unless (hash-ref (hash-ref payload 'author) 'bot #f)
                    (if (isowner? (author_id payload))
                            ;; Remove prefix, split at space
                            (let ([params (string-split (string-trim msg BOT_PREFIX #:right? #f) " ")])
                                (let ([cmd_name (first params)])
                                    (cond 
                                        [(string=? cmd_name "shell") (shell_cmd client payload params)]
                                        [else (http:create-message client (channel payload) (format "**Unknown command:** ~a" cmd_name))]
                                    )
                                )
                            )
                        
                        
                        (http:create-message client (channel payload) 
                            #:embed (make_embed 
                                #:title "Permission denied!"
                                #:description "You do __not__ have permission to use this command.\n\nIf you believe this was an error, it's probably not."
                                #:color 10038562
                            )
                        )
                    )  
                )
            )
        )
    )
)

;;; ____________________________________________________________________________________________________________________________________

(define dr (make-log-receiver discord-logger 'debug))
 
(thread
  (thunk
    (let loop ()
      (let ([v (sync dr)])
        (printf "[~a] ~a\n" (vector-ref v 0)
                (vector-ref v 1)))
      (loop))))
 
;;; ____________________________________________________________________________________________________________________________________

(start-client bot)
