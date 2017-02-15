(ns firmata.onewire
  (:require [firmata.core :refer [send-message]]
            [firmata.sysex :refer [consume-sysex]]
            [firmata.util :refer [msb lsb bytes-to-int]]
            [firmata.stream.spi :refer [read!]]
            [firmata.messages :refer [SYSEX_START SYSEX_END
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

(defn decode-7bit
  [b]
  (let [expected (bit-shift-right (* (count b) 7) 3)]
    (loop [i 0 acc '[]]
      (let [j (bit-shift-left i 3)
            pos (int (/ j 7))
            s (int (mod j 7))
            val (bit-or (bit-shift-right (nth b pos 0) s)
                        (bit-and (bit-shift-left (nth b (inc pos) 0) (- 7 s)) 0xFF))]
        (if (not= i expected)
          (recur (inc i) (conj acc val))
          (clojure.string/join "" (map #(format "%02X" %) acc)))))))

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
    {:type "onewire-search-reply"
     :addrs (map #(apply str %) (partition 16 decoded))
     }))

(defmethod read-sysex-event ONEWIRE_DATA
  [in]
  ;;
  (read-onewire-reply in))

(defn send-onewire-request
  "Sends a OneWire request with optional extended data"
  [board pin request & data]
  (let [msg (concat [SYSEX_START ONEWIRE_DATA request pin]
                    data
                    [SYSEX_END])]
    (send-message board msg)))

(defn enable-onewire
  ([board pin]
   (enable-onewire board pin false))
  ([board pin parasitic-power?]
   (let [msg (concat [SYSEX_START ONEWIRE_DATA ONEWIRE_CONFIG_REQUEST pin (if parasitic-power? 0x01 0x00) SYSEX_END])]
     (send-message board msg))))

(defn list-devices
  "Sends a OneWire search command and returns the addresses found"
  [board pin]
  (let [msg (concat [SYSEX_START ONEWIRE_DATA ONEWIRE_SEARCH_REQUEST pin SYSEX_END])]
    (send-message board msg)))
