(asdf:defsystem lc3
  :description "Little Computer 3 (LC3) Virtual Machine"
  :homepage "https://github.com/resttime/lc3"
  :bug-tracker "https://github.com/resttime/lc3/issues"
  :source-control (:git "https://github.com/resttime/lc3.git")
  ;; No build operation currently
  ;; :build-operation "asdf:program-op"
  ;; :build-pathname "lc3"
  ;; :entry-point "lc3:main"
  :serial t
  :components ((:file "lc3"))
  :depends-on (:cl-charms))
