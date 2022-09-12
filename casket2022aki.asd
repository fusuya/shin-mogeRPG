(defsystem #:casket2022aki
  :description ""
  :author "mogezou"
  :license  ""
  :version "0.0.2"
  :serial t
  :depends-on (#:ftw #:cffi)
  :components ((:module "src"
                :components
                  ((:file "package")
                   (:file "define")
                   (:file "item")
		   (:file "util")
                   (:file "donjon")
                   (:file "monster")
                   (:file "render")
                   (:file "main")))))
