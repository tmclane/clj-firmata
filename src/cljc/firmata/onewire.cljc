(ns firmata.onewire
  (:require [firmata.core :refer [send-message]]
            [firmata.sysex :refer [consume-sysex]]
            [firmata.util :refer [msb lsb bytes-to-int]]
            [firmata.stream.spi :refer [read!]]
            [firmata.messages :refer [SYSEX_START
                                      SYSEX_END
                                      ONEWIRE_CONFIG_REQUEST
                                      ONEWIRE_DATA
                                      ONEWIRE_DELAY_REQUEST_BIT
                                      ONEWIRE_READ_REPLY
                                      ONEWIRE_READ_REQUEST_BIT
                                      ONEWIRE_RESET_REQUEST_BIT
                                      ONEWIRE_SEARCH_ALARMS_REPLY
                                      ONEWIRE_SEARCH_ALARMS_REQUEST
                                      ONEWIRE_SEARCH_REPLY
                                      ONEWIRE_SEARCH_REQUEST
                                      ONEWIRE_WITHDATA_REQUEST_BITS
                                      ONEWIRE_WRITE_REQUEST_BIT]]
            [firmata.sysex :refer [read-sysex-event
                                   read-two-byte-data]]))

(defn addr2str
  "Converts an address into a human readable hex string"
  [addr]
  (clojure.string/join "" (map #(format "%02X" %) addr)))

(defn decode-7bit
  [buffer]
  (let [expected (bit-shift-right (* (count buffer) 7) 3)]
    (loop [i 0 acc '[]]
      (let [j (bit-shift-left i 3)
            pos (int (/ j 7))
            s (int (mod j 7))
            val (bit-or (bit-shift-right (nth buffer pos 0) s)
                        (bit-and (bit-shift-left (nth buffer (inc pos) 0) (- 7 s)) 0xFF))]
        (if (not= i expected)
          (recur (inc i) (conj acc val))
          acc)))))

(defn encode-7bit
  [buffer]
  (loop [[data & remainder] buffer
         shift 0
         previous 0
         acc '()]
    (if data
      (condp = shift
        0 (recur remainder
                 (inc shift)
                 (bit-shift-right data 7)
                 (conj acc (bit-and 0x7f data)))
        6 (recur remainder
                 0
                 previous
                 (conj acc
                       (bit-or previous
                               (bit-and (bit-shift-left data shift) 0x7f))
                       (bit-shift-right data 1)))
        (recur remainder
               (inc shift)
               (bit-shift-right data (- 8 (inc shift)))
               (conj acc
                       (bit-or previous
                               (bit-and (bit-shift-left data shift) 0x7f)))))
      (reverse (conj acc (if (> shift 0) previous))))))

(defmulti read-onewire-reply
  "Reads a onewire message.

  Returns a map with, at a minimum, the key :type.
  This should indicate what type of onewire message is being received.
  "
  (fn [in] (read! in)))

(defmethod read-onewire-reply ONEWIRE_SEARCH_REPLY
  [in]
  (let [pin (read! in)
        raw (consume-sysex in '[] #(conj %1 %2))
        decoded (decode-7bit raw)]
    {:type :onewire-search-reply
     :addrs (map #(apply str %) (partition 8 decoded))
     }))

(defmethod read-sysex-event ONEWIRE_DATA
  [in]
  ;;
  (read-onewire-reply in))

(defn- to-lsb-order
  "Converts a value to a LSB ordered list of bytes"
  ([value count]
   (loop [count count
          shift 0
          acc '()]
     (if (= count 0)
       (reverse acc)
       (recur (- count 2)
              (+ shift 16)
              (conj acc
                    (bit-and 0xFF (bit-shift-right value shift))
                    (bit-and 0xFF (bit-shift-right value (+ shift 8))))))))
  ([value]
   (to-lsb-order value 2)))

(defn send-onewire-request
  "Sends a OneWire request with optional extended data"
  ([board pin command]
   (send-onewire-request board pin command nil))
  ([board pin command data]
   (let [msg (concat [SYSEX_START ONEWIRE_DATA command pin]
                     data
                     [SYSEX_END])]
     (send-message board msg)))
  ([board pin command device correlationId bytesExpected delay data]
   (let [cmd (if (or device correlationId bytesExpected data delay)
               (bit-or command ONEWIRE_WITHDATA_REQUEST_BITS)
               command)
         addr (or device (take 8 (repeat 0)))
         bytesToRead (if bytesExpected
                         (to-lsb-order bytesExpected)
                         [0 0])
         correlationId (if correlationId
                         (to-lsb-order correlationId)
                         [0 0])
         delayValue (if delay
                      (to-lsb-order delay 4)
                      [0 0 0 0])
         payload (concat [addr
                          bytesToRead
                          correlationId
                          delayValue
                          data])]
     (send-onewire-request board pin cmd (encode-7bit payload)))))

(defn enable-onewire
  ([board pin]
   (enable-onewire board pin false))
  ([board pin parasitic-power?]
   (send-onewire-request board pin ONEWIRE_CONFIG_REQUEST
                         [(if parasitic-power? 0x01 0x00)])))

(defn list-devices
  "Sends a OneWire search command and returns the addresses found"
  [board pin]
  (send-onewire-request board pin ONEWIRE_SEARCH_REQUEST))
