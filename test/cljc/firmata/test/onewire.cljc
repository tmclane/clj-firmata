(ns firmata.test.onewire
  (:require #?(:clj
               [clojure.test :as t
                :refer (is deftest with-test run-tests testing)])
            [firmata.onewire :refer [decode-7bit
                                     encode-7bit]]))

(def encoded-addr '(40 100 76 3 102 0 0 0 127 81 52 94 1 82 1 0 0 110 3))
(def decoded-addr-binary '(40 50 115 96 6 0 0 255 40 205 27 144 6 0 0 247))
(def decoded-addr-hex ["28327360060000FF" "28CD1B90060000F7"])

(deftest ^:async test-encode-7bit
  (testing "Validate decoding 7bit data"
    (let [decoded (decode-7bit encoded-addr)
          encoded (encode-7bit decoded-addr-binary)]
      (is (= decoded decoded-addr-binary))
      (is (= encoded encoded-addr)))))
