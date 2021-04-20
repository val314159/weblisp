;;;; weblisp.lisp

(in-package #:weblisp)

(defvar *port* 8080)
(defvar *ws* nil "special variable for the current websocket")

(defclass chat-room (websocket-resource)
  ((name :initarg :name
	 :initform (error "Name this room!")
	 :reader name))
  (:default-initargs :client-class 'user))

(defclass user (websocket-client)
  ((name :initarg :user-agent
	 :reader name
	 :initform (error "Name this user!"))))

(defvar *chat-rooms* (list
		      (make-instance 'chat-room :name "/bongo")
		      (make-instance 'chat-room :name "/fury")))

(defun find-room (request)
  (find (script-name request) *chat-rooms* :test #'string= :key #'name))

(pushnew 'find-room *websocket-dispatch-table*)

(defun broadcast (room message &rest args)
  (loop for peer in (clients room) do
    (send-text-message peer (apply #'format nil message args))))

(defmethod client-connected ((room chat-room) user)
  (broadcast room "~a has joined ~a" (name user) (name room)))

(defmethod client-disconnected ((room chat-room) user)
  (broadcast room "~a has left ~a" (name user) (name room)))

(defmethod text-message-received ((room chat-room) user message)
  (broadcast room "~a says ~a" (name user) message))

(defvar *lib*
  `((defmacro subseq (seq &rest rest) `(chain ,seq (slice ,@rest)))

    (defmacro console-log (&rest rest)    `((@ console log) ,@rest))

    (defvar *location-href*     (@ location href))
    (defvar *location-origin*   (@ location origin))

    (defmacro document-body ()            ` (@ document body))
    (defmacro  first-child (elt1)         ` (@ ,elt1 first-child))
    (defmacro remove-child (elt1 elt2)    `((@ ,elt1 remove-child) ,elt2))
    (defmacro append-child (elt1 elt2)    `((@ ,elt1 append-child) ,elt2))
    (defmacro   push-child (elt1 elt2)    `((@ ,elt2 append-child) ,elt1))

    (defmacro       data       (elt) `      (@ ,elt data))
    (defmacro       code       (elt) `      (@ ,elt code))
    (defmacro       websocket  (url) `(new (-web-socket ,url)))
    (defmacro       value      (elt) `      (@ ,elt value))
    (defmacro       oncls       (ws) `      (@ ,ws onclose))
    (defmacro       onerr       (ws) `      (@ ,ws onerror))
    (defmacro       onopn       (ws) `      (@ ,ws onopen))
    (defun          onmsg       (ws)        (@  ws onmessage))
    (defun    (setf onmsg)   (fn ws)  (setf (@  ws onmessage)
					    (lambda (e) ( fn (data e) e))))
    (defun get-ws-url (path)
      (concatenate 'string
		   "ws"
		   (subseq *location-href* 4)
		   path))
    (defun make-websocket (path open close message error)
      (let ((ws (websocket (get-ws-url path))))
	(setf (onopn ws) open)
	(setf (oncls ws) close)
	(setf (onmsg ws) message)
	(setf (onerr ws) error)
	ws))
    (defun make-element (s attrs body)
      (let ((elt (chain document (create-element s))))
	(setf (inner-html elt) body)
	elt))
    (defun make-text-node (s)
      (chain document (create-text-node s)))
    ))

(defvar *server* (make-instance 'websocket-easy-acceptor :port *port*))

(defun code-fury ()
  `(progn
     ,@*lib*
     (defvar ws (make-websocket ""
				(lambda (x) ((@ console log) 1 x))
				(lambda (x) ((@ console log) 2 x))
				(lambda (x) ((@ console log) 3 (data x)))
				(lambda (x) ((@ console log) 4 x))
				))))

(defun code-bongo ()
  `(progn
     ,@*lib*
     (defun send (s)
       ((@ ws send) s))
     (defun output (s)
       (push-child (make-text-node s)  (document-body))
       (push-child (make-element "br") (document-body))
       s)
     (defun process (s)
       (send s))
     (defun keydown (elt)
       (if (eql (code event) "Tab")
	   (return-from keydown false))
       (if (eql (code event) "Enter")
	   (setf (value elt) (or (process (value elt)) "")))
       t)
     (defvar ws (make-websocket ""
				(lambda (x)   ((@ console log) 1 x))
				(lambda (x)   ((@ console log) 2 x))
				(lambda (s x) (console-log 3 "msg:" (output s)))
				(lambda (x)   ((@ console log) 4 x))
				))))

(define-easy-handler (fury :uri "/fury") ()
  (who-ps-html
   (:script (ps* (code-fury)))))

(define-easy-handler (bongo :uri "/bongo") ()
  (who-ps-html
   (:html
    (:head (:link :rel "stylesheet"
		  :href "https://unpkg.com/purecss@2.0.5/build/pure-min.css")
	   (:meta :name "viewport"
		  :content "width=device-width, initial-scale=1"))
    (:body
     (:input
      :style "width:100%"
      :onkeydown (ps (keydown this))
      :autofocus t)
     (:script (ps* (code-bongo)))))))

(defun spin-up ()
  (if (started-p *server*)
      (format t "already started!~%")
      (format t "starting ~s!~%" (start *server*))))

(defun spin-down ()
  (when (started-p *server*)
    (format t "stopping!~%")
    (stop *server*)))
