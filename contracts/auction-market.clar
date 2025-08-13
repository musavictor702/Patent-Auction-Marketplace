
;; title: auction-market
;; version:
;; summary:
;; description:


(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-auction-exists (err u102))
(define-constant err-auction-not-active (err u103))
(define-constant err-auction-ended (err u104))
(define-constant err-bid-too-low (err u105))
(define-constant err-cannot-cancel (err u106))
(define-constant err-not-bidder (err u107))
(define-constant err-auction-not-ended (err u108))
(define-constant err-already-claimed (err u109))
(define-constant err-invalid-input (err u110))

(define-data-var next-auction-id uint u1)

(define-map auctions
  { auction-id: uint }
  {
    patent-id: (string-ascii 64),
    creator: principal,
    description: (string-utf8 500),
    start-block: uint,
    end-block: uint,
    reserve-price: uint,
    highest-bid: uint,
    highest-bidder: (optional principal),
    status: (string-ascii 20),
    claimed: bool
  }
)

(define-map bids
  { auction-id: uint, bidder: principal }
  { amount: uint }
)

(define-map patent-owners
  { patent-id: (string-ascii 64) }
  { owner: principal }
)

(define-read-only (get-auction (auction-id uint))
  (match (map-get? auctions { auction-id: auction-id })
    auction-data (ok auction-data)
    (err err-not-found)
  )
)

(define-read-only (get-bid (auction-id uint) (bidder principal))
  (default-to { amount: u0 }
    (map-get? bids { auction-id: auction-id, bidder: bidder })
  )
)

(define-read-only (get-patent-owner (patent-id (string-ascii 64)))
  (match (map-get? patent-owners { patent-id: patent-id })
    owner-data owner-data
    { owner: contract-owner }
  )
)

(define-read-only (get-auction-status (auction-id uint))
  (match (map-get? auctions { auction-id: auction-id })
    auction-data (ok (get status auction-data))
    (err err-not-found)
  )
)

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? auctions { auction-id: auction-id })
    auction-data 
      (and 
        (is-eq (get status auction-data) "active")
        (>= stacks-block-height (get start-block auction-data))
        (<= stacks-block-height (get end-block auction-data))
      )
    false
  )
)

(define-read-only (get-next-auction-id)
  (var-get next-auction-id)
)



(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-not-found))
      (current-highest-bid (get highest-bid auction))
      (min-bid (if (> current-highest-bid u0)
                  (+ current-highest-bid u1)
                  (get reserve-price auction)))
    )
    (asserts! (is-auction-active auction-id) err-auction-not-active)
    (asserts! (>= bid-amount min-bid) err-bid-too-low)
    
    (map-set bids
      { auction-id: auction-id, bidder: tx-sender }
      { amount: bid-amount }
    )
    
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        highest-bid: bid-amount,
        highest-bidder: (some tx-sender)
      })
    )
    
    (ok bid-amount)
  )
)

(define-public (cancel-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator auction)) err-owner-only)
    (asserts! (is-none (get highest-bidder auction)) err-cannot-cancel)
    
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        status: "cancelled"
      })
    )
    
    (ok true)
  )
)

(define-public (end-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator auction)) err-owner-only)
    (asserts! (is-eq (get status auction) "active") err-auction-not-active)
    (asserts! (>= stacks-block-height (get end-block auction)) err-auction-not-ended)
    
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        status: "ended"
      })
    )
    
    (ok true)
  )
)

(define-public (claim-patent (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-not-found))
      (winner (unwrap! (get highest-bidder auction) err-not-found))
    )
    (asserts! (is-eq tx-sender winner) err-not-bidder)
    (asserts! (is-eq (get status auction) "ended") err-auction-not-ended)
    (asserts! (not (get claimed auction)) err-already-claimed)
    
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        claimed: true
      })
    )
    
    (map-set patent-owners
      { patent-id: (get patent-id auction) }
      { owner: tx-sender }
    )
    
    (ok true)
  )
)

(define-public (withdraw-bid (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-not-found))
      (bid-info (get-bid auction-id tx-sender))
      (highest-bidder (get highest-bidder auction))
    )
    (asserts! (or 
                (is-eq (get status auction) "ended") 
                (is-eq (get status auction) "cancelled")) 
              (err u103))
    (asserts! (> (get amount bid-info) u0) err-not-found)
    (asserts! (or 
                (is-eq (get status auction) "cancelled")
                (is-some highest-bidder)
                (not (is-eq (some tx-sender) highest-bidder))) 
              err-not-bidder)
    
    (map-delete bids { auction-id: auction-id, bidder: tx-sender })
    
    (ok true)
  )
)


(define-constant time-extension  u100)
(define-constant extension-threshold  u10)

(define-public (place-bid-with-extension (auction-id uint) (bid-amount uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) err-not-found))
      (current-highest-bid (get highest-bid auction))
      (min-bid (if (> current-highest-bid u0)
                  (+ current-highest-bid u1)
                  (get reserve-price auction)))
      (blocks-remaining (- (get end-block auction) stacks-block-height))
    )
    (asserts! (is-auction-active auction-id) err-auction-not-active)
    (asserts! (>= bid-amount min-bid) err-bid-too-low)
    
    (map-set bids
      { auction-id: auction-id, bidder: tx-sender }
      { amount: bid-amount }
    )
    
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        highest-bid: bid-amount,
        highest-bidder: (some tx-sender),
        end-block: (if (<= blocks-remaining extension-threshold)
                      (+ (get end-block auction) time-extension)
                      (get end-block auction))
      })
    )
    
    (ok bid-amount)
  )
)


(define-public (create-multiple-auctions 
    (patent-ids (list 10 (string-ascii 64)))
    (descriptions (list 10 (string-utf8 500)))
    (durations (list 10 uint))
    (reserve-prices (list 10 uint)))
  (let
    (
      (auction-count (len patent-ids))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq auction-count (len descriptions)) err-invalid-input)
    (asserts! (is-eq auction-count (len durations)) err-invalid-input)
    (asserts! (is-eq auction-count (len reserve-prices)) err-invalid-input)
    
    (ok (map create-single-auction patent-ids descriptions durations reserve-prices))
  )
)

(define-private (create-single-auction 
    (patent-id (string-ascii 64))
    (description (string-utf8 500))
    (duration uint)
    (reserve-price uint))
  (let
    (
      (auction-id (var-get next-auction-id))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration))
    )
    (map-set auctions
      { auction-id: auction-id }
      {
        patent-id: patent-id,
        creator: tx-sender,
        description: description,
        start-block: start-block,
        end-block: end-block,
        reserve-price: reserve-price,
        highest-bid: u0,
        highest-bidder: none,
        status: "active",
        claimed: false
      }
    )
    (map-set patent-owners
      { patent-id: patent-id }
      { owner: contract-owner }
    )
    (var-set next-auction-id (+ auction-id u1))
    auction-id
  )
)


(define-public (create-auction 
    (patent-id (string-ascii 64))
    (description (string-utf8 500))
    (duration uint)
    (reserve-price uint))
  (let
    (
      (auction-id (var-get next-auction-id))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set auctions
      { auction-id: auction-id }
      {
        patent-id: patent-id,
        creator: tx-sender,
        description: description,
        start-block: start-block,
        end-block: end-block,
        reserve-price: reserve-price,
        highest-bid: u0,
        highest-bidder: none,
        status: "active",
        claimed: false
      }
    )
    (map-set patent-owners
      { patent-id: patent-id }
      { owner: contract-owner }
    )
    (var-set next-auction-id (+ auction-id u1))
    (try! (contract-call? .auction-search register-auction auction-id))
    (ok auction-id)
  )
)