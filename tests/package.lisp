(defpackage #:rouse/tests
  (:use #:cl #:fiveam))


(in-package #:rouse/tests)

(fiveam:def-suite :rouse-tests)
(fiveam:def-suite :rouse-tests.simulation :in :rouse-tests)
(fiveam:def-suite :rouse-tests.viewer :in :rouse-tests)
