(ql:quickload '(:alexandria :cl-ppcre :iterate :str :binding-arrows))
(use-package :binding-arrows)

(defconstant +passport-flags+ '(:byr :iyr :eyr :hgt :hcl :ecl :pid :cid))
(defconstant +required-flags+ '(:byr :iyr :eyr :hgt :hcl :ecl :pid))

(defun has-reqd-fields? (passport)
  "Make sure the passport has all of the required fields."
  (let ((flags (mapcar #'first passport)))
    (every (lambda (req) (find req flags :test #'equal)) +required-flags+)))

|=
(has-reqd-fields? '((:ecl "gry")
                    (:pid "860033327")
                    (:eyr "2020")
                    (:hcl "#fffffd")
                    (:byr "1937")
                    (:iyr "2017")
                    (:cid "147")
                    (:hgt "183cm")))
=|

(defun between (min max val)
  (<= min val max))

(defun parse-height (height)
  (let ((cm (search "cm" height))
        (in (search "in" height)))
    (cond (cm (between 150 193 (parse-integer (subseq height 0 cm))))
          (in (between 59  76  (parse-integer (subseq height 0 in))))
          (:otherwise nil))))

(defun passport-dispatch (field)
  (destructuring-bind (flag val) field
    (case flag
      (:byr (between 1920 2002 (parse-integer val)))
      (:iyr (between 2010 2020 (parse-integer val)))
      (:eyr (between 2020 2030 (parse-integer val)))
      (:hgt (parse-height val))
      (:hcl (ppcre:scan "^\\#[0-9a-fA-F]{6}$" val))
      (:ecl (ppcre:scan "amb|blu|brn|gry|grn|hzl|oth" val))
      (:pid (ppcre:scan "^\\d{9}$" val))
      (:cid t))))

(defun has-valid-info? (passport)
  (every #'passport-dispatch passport))

|=
(has-valid-info?  '((:ecl "gry")
                    (:pid "860033327")
                    (:eyr "2020")
                    (:hcl "#fffffd")
                    (:byr "1937")
                    (:iyr "2017")
                    (:cid "147")
                    (:hgt "183cm")))
=|

(defun valid-passport? (passport)
  "Passport is list of parameters. Parameters are a list,
1st parameter is the key, second is the value.
There are two checks:
1: Are all of the necessary parameters present?
2: Are all of the values valid?"
  (and
   (has-reqd-fields? passport)
   (has-valid-info?  passport)))

(defun symbolize (s)
  "Turn a string into a keyword"
  (intern (string-upcase s) (find-package :keyword)))

(defun create-passport (passport-str)
  "Creates list of parameters from passport string. Format ((par val) (par val) ...)
We don't do any checks at this point, just take in all the data."
  (->> passport-str
    (str:split " ")
    (mapcar (lambda (s) (str:split ":" s)))
    (mapcar (lambda (s) (list (symbolize (first s)) (second s))))))

(defun read-input (filename)
  "Read in our input, return passports as list passports."
  (let ((nlnl (coerce '(#\Newline #\Newline) 'string)) ; Each passport is seperated by an empty line
        (nl (coerce '(#\Newline) 'string)))            ; and I don't know how to replace 2 chars
    (->> filename
      (uiop:read-file-string)
      (str:trim)
      (str:split nlnl)
      (mapcar (lambda (s) (str:replace-all nl " " s)))
      (mapcar #'create-passport))))

(defun day4-solver (filename)
  (->> filename
    read-input
    (count-if #'valid-passport?)))

(day4-solver "day4-test")
(day4-solver "day4-input")


; (= (length (read-input "day4-test")) 12)
