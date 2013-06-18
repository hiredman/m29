(ns com.thelastcitadel.m29
  (:require [clj-http.client :as http]
            [cheshire.core :as json]))

(defn random-key []
  (-> (doto (javax.crypto.KeyGenerator/getInstance "AES")
        (.init 128))
      (.generateKey)))

(defn key-bytes [key]
  (.getEncoded key))

(defn encode-key [key]
  (.encode (sun.misc.BASE64Encoder.) (key-bytes key)))

(defn cipher [key]
  (doto (javax.crypto.Cipher/getInstance "AES/ECB/PKCS5Padding")
    (.init javax.crypto.Cipher/ENCRYPT_MODE key)))

(defn m29-1 []
  (let [k (random-key)]
    {:key k
     :key-bytes (key-bytes k)
     :cipher (cipher k)}))

(defn m29-2 [{:keys [cipher] :as m} url]
  (assoc m
    :long-url (.doFinal cipher (.getBytes url))))

(defn m29-3 [{:keys [long-url key-bytes] :as m}]
  (let [fk (into-array Byte/TYPE (take 8 key-bytes))
        sk (into-array Byte/TYPE (drop 8 key-bytes))]
    (assoc m
      :first-key fk
      :second-key sk)))

(defn m29-4 [{:keys [long-url first-key second-key key-bytes] :as m}]
  (letfn [(encode [bytes]
            (-> (.encode (sun.misc.BASE64Encoder.) bytes)
                (.replaceAll "\\+" "-")
                (.replaceAll "/" "_")
                (.replaceAll "=" "")))]
    (assoc m
      :long-url (encode long-url)
      :first-key (encode first-key)
      :second-key (encode second-key))))

(defn m29-5 [{:keys [long-url first-key second-key]}]
  (-> (http/post "http://api.m29.us/urlshortener/v1/url"
                 {:content-type :json
                  :body (json/encode {:longUrlEncrypted long-url
                                      :firstKey first-key})
                  :as :json})
      :body
      :id
      (str "/" second-key)))

(defn m29 [url]
  (-> (m29-1)
      (m29-2 url)
      (m29-3)
      (m29-4)
      (m29-5)))

(defn shorten [url]
  (m29 url))
