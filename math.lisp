;;;; An expandable math app written in Common Lisp
(defvar *operator-list* (list
                         '(:operator "(" :order nil) ; Parens are special, just need to be here for parsing.
                         '(:operator ")" :order nil)
                         '(:operator "," :order nil)
                         '(:operator "^" :function ^ :order 1)
                         '(:operator "*" :function * :order 2)
                         '(:operator "/" :function special-div :order 3)
                         '(:operator "%" :function % :order 4)
                         '(:operator "+" :function + :order 5)
                         '(:operator "-" :function special-subtract :order 6))
  "List of supported operators and associated functions. Function must take any number of arguments 0-n. With only 1 arg, the value must be returned simply.")
(defvar *db* nil
  "Database of user configured variables and functions")
(defvar *input-string* nil
  "Store the latest equation passed into the program")
(defvar *db-location* "~/.cache/.cl-math"
  "Where the database is stored")

;;;; Division and subtraction by default do different things than returning the lone
;;;; value if called with a single argument, need to redefine them to not do that.
(defun special-div (&rest numbers)
  "Division function except single args return the value passed in."
  (if (> (length numbers) 1)
      (apply '/ numbers)
      (if (eql (length numbers) 1)
          (pop numbers)
          1)))

(defun special-subtract (&rest numbers)
  "Subtract function except single args return the value passed in."
  (if (> (length numbers) 1)
      (apply '- numbers)
      (if (eql (length numbers) 1)
          (pop numbers)
          1)))

;;;; modulus function to make mod behave like other operators
(defun % (&rest numbers)
  "Runs `mod' against any amount of numbers, returns 1 if nothing provided."
  (if (> (length numbers) 1)
      (let ((result (mod (pop numbers) (pop numbers))))
        (loop while numbers
              do (setf result (mod result (pop numbers))))
        result)
    (if (eql (length numbers) 1)
        (pop numbers)
      1)))

;;;; Functions to generate the unlimited arg pow
(defun repeat-number (number count)
  "Returns a list with NUMBER repeated COUNT times"
  (loop for num-count from (1- count) downto 0
        collect number))

(defun pow (x y)
  "Return x to the y power"
  (apply '* (repeat-number x y)))

(defun ^ (&rest numbers)
  "Runs `pow' against any amount of numbers, returns 1 when called alone"
  (if (> (length numbers) 1)
      (let ((numbers-reversed (reverse numbers)))
        (let ((result (apply 'pow (reverse (list (pop numbers-reversed) (pop numbers-reversed))))))
          (loop while numbers-reversed
                do (setf result (pow (pop numbers-reversed) result)))
          result))
    (if (eql (length numbers) 1)
        (pop numbers)
      1)))


;;;; Functions to process a math input string
(defun remove-spaces (str)
  "Removes all spaces from a string"
  (remove-if #'(lambda (chr) (equal chr #\ )) str))

(defun make-comparison-expr (field value)
  "Build a specific equal check based on a :field and a value for that field."
  `(equal (getf object ,field) ,value))

(defun make-comparisons-list (fields)
  "Build a list of generic equal checks based on a :field and a value."
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro fields (&rest fields)
  "Check if a value of a field matches in the given list. (can give any amount of fields and values)"
  `#'(lambda (object) (and ,@(make-comparisons-list fields))))

(defun field-val-in-list (list fields-fn)
  "Check if a value of a field matches in the given list. Generate fields-fn with fields macro"
  (> (length (remove-if-not fields-fn list))
     0))

(defun is-operator (character)
  "Check if a given character or word is an operator."
  (field-val-in-list *operator-list* (fields :operator (string character))))

(defun is-not-operator (character)
  "Check if a given character is not an operator."
  (not (field-val-in-list *operator-list* (fields :operator (string character)))))

(defun is-number (character)
  "Check if a given character is a number (float)."
  (if (find (string character)
            (list "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ".")
            :test #'equal)
      t nil))

(defun word-is-number (str)
  "Check if a given word is a number (float)."
  (not (loop for c across str do (if (not (is-number c)) (return t)))))

(defun word-is-variable (str)
  "Check if a given string is a variable."
  (field-val-in-list *db* (fields :name (string str) :type "variable")))

(defun word-is-function (str)
  "Check if a given string is a function."
  (field-val-in-list *db* (fields :name (string str) :type "function")))

(defun collect-word (check-function equation start)
  "Collect a word from the string using a specific check function. Collects till the check function fails."
  (let ((the-word "")
        (trimmed-equation (subseq equation start (length equation))))
    ;; Check each character against the check function, return if the check function fails
    (loop for c across trimmed-equation
          do (if (funcall check-function c)
                 (setf the-word (concatenate 'string the-word (string c)))
                 (return the-word)))
    ;; Return the full string if the full string matched the check function
    the-word))

(defun collect-char (check-function equation start)
  "Collect a character as a string from the string using a specific check function."
  (if (funcall check-function (char equation start))
      (string (char equation start))
      nil))

;; TODO clean this function up and support negative numbers
(defun math-string-to-list (input)
  "Isolate each part of a math string into a string element in a list."
  (let ((curr-value "")
        (input-index 0)
        (input-trimmed (remove-spaces input)) ; Remove all spaces from the string before parsing
        (input-list))
    ;;(format t "Trimmed input: ~a~%" input-trimmed)
    (loop 
      ;; Try collecting a number
      (setf curr-value (collect-word 'is-number input-trimmed input-index))
      ;; If there wasn't a number, try collecting an operator
      (when (or (equal curr-value "") (equal curr-value nil))
          (setf curr-value (collect-char 'is-operator input-trimmed input-index))
          ;; Check for variables or functions
          (when (or (equal curr-value "") (equal curr-value nil))
              (setf curr-value (collect-word 'is-not-operator input-trimmed input-index))))
      ;; Debug statement
      ;; (format t "~a: ~a => ~a (~a)~%" curr-value input-index (+ input-index (max 1 (length curr-value))) (length input-trimmed))
      (push curr-value input-list)
      (setf input-index (+ input-index (max 1 (length curr-value))))
      (if (not (< input-index (length input-trimmed)))
          (return (reverse (remove-if #'(lambda (str) (or (equal str "") (equal str nil))) input-list)))))))

(defun valid-math (obj1 obj2)
  "Check if 2 objects are in correct math order"
  (if (or (and (is-operator obj1) (or (is-not-operator obj2) (equal "(" obj2)))
          (and (or (is-not-operator obj1) (equal ")" obj1)) (is-operator obj2)))
      t))

(defun last-string (list)
  "Get the last string in a list of strings"
  (nth (- (length list) 1) list))

(defun count-parens (list)
  "Count parenthesis in a list"
  (length (remove-if-not #'(lambda (val) (or (equal "(" val) (equal ")" val))) list)))

(defun validate-math-list (math-list)
  "Check if the math list has valid syntax (NUMBER OPERATOR NUMBER)"
  (if (and (> (length math-list) 0)
           (or (equal ")" (last-string math-list))
               (word-is-number (last-string math-list)))
           (equal 0 (mod (count-parens math-list) 2)))
      (let ((last-obj (nth 0 math-list)))
        (not (loop for obj-idx from 0 to (- (length math-list) 1)
               do (if (valid-math last-obj (nth obj-idx math-list))
                      (setf last-obj (nth obj-idx math-list))
                      (return t)))))))

(defun get-max-oper-order ()
  "Get max operator order from the global *operator-list*."
  (apply 'max
         (let ((op-list *operator-list*))
           (remove-if #'(lambda (obj) (equal obj nil))
                      (loop while op-list collect (getf (pop op-list) :order))))))

(defun parse-float (string)
  "Parse a string to a float. Returns 0 if the string is not a valid float."
  (if (word-is-number string)
      (let ((*read-eval* nil))
        (with-input-from-string (stream string)
          (read stream nil nil)))
      0))

(defun trim-nils (list)
  "Remove nil entries in a list"
  (remove-if #'(lambda (val) (equal val nil)) list))

(defun collect-numbers-for-oper (operator-str math-list)
  "Collect numbers matching the operator string until a non-matching operator is seen"
  (if (> (length math-list) 0)
      (let ((number-list (list (parse-float (pop math-list)))))
        (loop while math-list
              do (let ((word (pop math-list)))
                   (if (not (equal operator-str word))
                       (if (word-is-number word)
                           (push (parse-float word) number-list)
                           (return (list :list (push word math-list) :numbers (reverse number-list)))))))
        (list :list math-list :numbers (reverse number-list)))))

(defun solve-for-operator (operator math-list)
  "Solve math for the given operator, return the math list with all math for the given operator solved."
  (if (> (length math-list) 0)
      (let ((oper-value (getf operator :operator))
            (oper-function (getf operator :function))
            (parsed-list nil))
        (loop while math-list
              do (let ((collection-results (collect-numbers-for-oper oper-value math-list)))
                   (setf math-list (getf collection-results :list))
                   (setf parsed-list (concatenate 'list
                                                  (list (pop math-list)
                                                        (write-to-string (apply oper-function (getf collection-results :numbers))))
                                                  parsed-list))))
        (trim-nils (reverse parsed-list)))))

(defun solve-simple-math-list (math-list)
  "Solve a simple math list (does not parse parenthesis and functions)"
  (if (equal 1 (length math-list))
      (parse-float (last-string math-list))
      (let ((parsed-list math-list))
        (loop for oper-order from 0 to (get-max-oper-order)
             do (let ((operators (remove-if-not (fields :order oper-order) *operator-list*)))
                  (loop while operators
                        do (setf parsed-list (solve-for-operator (pop operators) parsed-list)))))
        (parse-float (last-string parsed-list)))))

(defun parse-inner-parenthesis (math-list)
  "Parses math up to a closing parenthesis and returns the resulting list with the parenthesis parsed."
  (let ((parsed-list nil))
    (loop while math-list
          do (let ((this-word (pop math-list)))
               (if (equal this-word ")")
                   (return (list :after math-list :paren-list (trim-nils (reverse parsed-list))))
                   (if (equal this-word "(")
                       (let ((recursed-list (parse-inner-parenthesis math-list)))
                         (push (write-to-string (solve-simple-math-list (getf recursed-list :paren-list))) parsed-list)
                         (setf math-list (getf recursed-list :after)))
                       (push this-word parsed-list)))))
    (list :after math-list :paren-list (trim-nils (reverse parsed-list)))))

(defun process-math-list (math-list)
  "Processes math with parenthesis. No support for variables and functions."
  (solve-simple-math-list (getf (parse-inner-parenthesis math-list) :paren-list)))

(defun process-math (math-string)
  "Process a math string."
  (let ((math-list (math-string-to-list math-string)))
    (if (validate-math-list math-list)
        (process-math-list math-list)
        (format t "~a " "Invalid math"))))


;;;; Database management
(defun set-variable (variable-name value)
  (push (list :type "variable" :name variable-name :value value) *db*)
  (save-db *db-location*))

;; TODO NOT IMPLEMENTED
(defun set-function (function-name func)
  (push (list :type "function" :name function-name :value func) *db*)
  (save-db *db-location*))

(defun save-db (filename)
  "Save the database to a file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
                  ;; with-standard-io-syntax ignores all print settings so that context is easily repeated
                  (with-standard-io-syntax
                   (print *db* out))))

(defun load-db (filename)
  "Load the cd database from a file"
  (ignore-errors
   (with-open-file (in filename
                       :if-does-not-exist :error)
     (with-standard-io-syntax
       (setf *db* (read in))))))


;;;; User interface
(defun prompt-read (prompt)
  "Get information from a user"
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun print-help ()
  "Print the help screen for this app"
  (format t "cl-math [-v] \"1 + 1\"~%Interactive mode is entered when you do not pass math as an argument.~%Interactive commands:~%quit :: Quit interactive mode~%help :: Show this help info~%defun :: Define a math function~%For more information see the README."))

;; TODO NOT IMPLEMENTED
(defun interactive-set-function ()
  "Interactively create a function."
  (let ((function-name (prompt-read "Function name: "))
        (variables (prompt-read "Variables, comma separated: "))
        (function-math (prompt-read "Formula: ")))
    (if (validate-math-list function-math)
        (set-function function-name nil))))

(defun interactive-mode ()
  "Enter an interactive session parsing math."
  (loop (let ((user-input (prompt-read "> ")))
          (cond ((equal user-input "quit") (return))
                ((equal user-input "help") (print-help))
                ((equal user-input "defun") (interactive-set-function))
                (t (format t "~a~%" (process-math user-input)))))))
