;; Patent Royalty Distribution System
;; Manages automatic royalty distribution for patent auction proceeds

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u400))
(define-constant err-not-found (err u401))
(define-constant err-invalid-percentage (err u402))
(define-constant err-unauthorized (err u403))
(define-constant err-already-distributed (err u404))
(define-constant err-invalid-beneficiary (err u405))
(define-constant err-no-royalties (err u406))

;; Maximum number of beneficiaries per patent
(define-constant max-beneficiaries u10)

;; Royalty configuration for each patent
(define-map patent-royalties
  { patent-id: (string-ascii 64) }
  {
    creator: principal,
    total-percentage: uint,
    beneficiary-count: uint,
    created-at: uint,
    active: bool
  }
)

;; Individual beneficiary details
(define-map royalty-beneficiaries
  { patent-id: (string-ascii 64), beneficiary-index: uint }
  {
    recipient: principal,
    percentage: uint,
    name: (string-utf8 50),
    role: (string-ascii 20)
  }
)

;; Distribution records for each auction
(define-map auction-distributions
  { auction-id: uint }
  {
    patent-id: (string-ascii 64),
    total-amount: uint,
    distributed: bool,
    distributed-at: uint,
    distribution-count: uint
  }
)

;; Individual payment records
(define-map distribution-payments
  { auction-id: uint, payment-index: uint }
  {
    recipient: principal,
    amount: uint,
    percentage: uint,
    paid-at: uint
  }
)

;; Beneficiary earnings tracking
(define-map beneficiary-earnings
  { recipient: principal }
  {
    total-earned: uint,
    payment-count: uint,
    last-payment: uint
  }
)

;; Global statistics
(define-data-var total-distributions uint u0)
(define-data-var total-royalties-paid uint u0)

;; Read-only functions
(define-read-only (get-royalty-config (patent-id (string-ascii 64)))
  (match (map-get? patent-royalties { patent-id: patent-id })
    config (ok config)
    (err err-not-found)
  )
)

(define-read-only (get-beneficiaries (patent-id (string-ascii 64)))
  (let
    ((config (unwrap! (map-get? patent-royalties { patent-id: patent-id }) err-not-found))
     (count (get beneficiary-count config)))
    (ok (map get-single-beneficiary 
      (generate-indices count)
      (list patent-id patent-id patent-id patent-id patent-id 
            patent-id patent-id patent-id patent-id patent-id)))
  )
)

(define-read-only (get-distribution-status (auction-id uint))
  (match (map-get? auction-distributions { auction-id: auction-id })
    distribution (ok distribution)
    (err err-not-found)
  )
)

(define-read-only (get-beneficiary-earnings (recipient principal))
  (ok (default-to 
    { total-earned: u0, payment-count: u0, last-payment: u0 }
    (map-get? beneficiary-earnings { recipient: recipient })))
)

(define-read-only (get-distribution-stats)
  (ok {
    total-distributions: (var-get total-distributions),
    total-royalties-paid: (var-get total-royalties-paid)
  })
)

;; Public functions
(define-public (setup-royalty-distribution 
    (patent-id (string-ascii 64))
    (beneficiaries (list 10 { recipient: principal, percentage: uint, name: (string-utf8 50), role: (string-ascii 20) })))
  (let
    ((beneficiary-count (len beneficiaries))
     (total-percentage (fold calculate-total-percentage beneficiaries u0)))
    
    (asserts! (> beneficiary-count u0) err-invalid-beneficiary)
    (asserts! (<= beneficiary-count max-beneficiaries) err-invalid-beneficiary)
    (asserts! (is-eq total-percentage u100) err-invalid-percentage)
    
    (map-set patent-royalties
      { patent-id: patent-id }
      {
        creator: tx-sender,
        total-percentage: total-percentage,
        beneficiary-count: beneficiary-count,
        created-at: stacks-block-height,
        active: true
      })
    
    (ok (setup-beneficiary-records patent-id beneficiaries))
  )
)

(define-public (distribute-auction-proceeds (auction-id uint) (patent-id (string-ascii 64)) (total-amount uint))
  (let
    ((royalty-config (unwrap! (map-get? patent-royalties { patent-id: patent-id }) err-not-found))
     (existing-distribution (map-get? auction-distributions { auction-id: auction-id })))
    
    (asserts! (is-none existing-distribution) err-already-distributed)
    (asserts! (get active royalty-config) err-no-royalties)
    (asserts! (> total-amount u0) err-invalid-percentage)
    
    (map-set auction-distributions
      { auction-id: auction-id }
      {
        patent-id: patent-id,
        total-amount: total-amount,
        distributed: true,
        distributed-at: stacks-block-height,
        distribution-count: (get beneficiary-count royalty-config)
      })
    
    (var-set total-distributions (+ (var-get total-distributions) u1))
    (var-set total-royalties-paid (+ (var-get total-royalties-paid) total-amount))
    (ok (process-royalty-payments auction-id patent-id total-amount (get beneficiary-count royalty-config)))
  )
)

(define-public (update-royalty-config (patent-id (string-ascii 64)) (active bool))
  (let
    ((config (unwrap! (map-get? patent-royalties { patent-id: patent-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get creator config)) err-unauthorized)
    
    (map-set patent-royalties
      { patent-id: patent-id }
      (merge config { active: active }))
    
    (ok true)
  )
)

;; Private helper functions
(define-private (calculate-total-percentage 
    (beneficiary { recipient: principal, percentage: uint, name: (string-utf8 50), role: (string-ascii 20) })
    (total uint))
  (+ total (get percentage beneficiary))
)

(define-private (setup-beneficiary-records
    (patent-id (string-ascii 64))
    (beneficiaries (list 10 { recipient: principal, percentage: uint, name: (string-utf8 50), role: (string-ascii 20) })))
  (fold store-beneficiary-with-index
    beneficiaries
    { patent-id: patent-id, index: u0 })
)

(define-private (store-beneficiary-with-index
    (beneficiary { recipient: principal, percentage: uint, name: (string-utf8 50), role: (string-ascii 20) })
    (acc { patent-id: (string-ascii 64), index: uint }))
  (begin
    (map-set royalty-beneficiaries
      { patent-id: (get patent-id acc), beneficiary-index: (get index acc) }
      {
        recipient: (get recipient beneficiary),
        percentage: (get percentage beneficiary),
        name: (get name beneficiary),
        role: (get role beneficiary)
      })
    { patent-id: (get patent-id acc), index: (+ (get index acc) u1) })
)

(define-private (process-royalty-payments
    (auction-id uint)
    (patent-id (string-ascii 64))
    (total-amount uint)
    (beneficiary-count uint))
  (fold make-single-payment
    (generate-indices beneficiary-count)
    { auction-id: auction-id, patent-id: patent-id, total-amount: total-amount, success: true })
)

(define-private (make-single-payment
    (index uint)
    (acc { auction-id: uint, patent-id: (string-ascii 64), total-amount: uint, success: bool }))
  (let
    ((beneficiary (map-get? royalty-beneficiaries { patent-id: (get patent-id acc), beneficiary-index: index })))
    (match beneficiary
      beneficiary-data 
        (let
          ((payment-amount (/ (* (get total-amount acc) (get percentage beneficiary-data)) u100))
           (recipient (get recipient beneficiary-data)))
          
          (map-set distribution-payments
            { auction-id: (get auction-id acc), payment-index: index }
            {
              recipient: recipient,
              amount: payment-amount,
              percentage: (get percentage beneficiary-data),
              paid-at: stacks-block-height
            })
          
          (let
            ((current-earnings (default-to { total-earned: u0, payment-count: u0, last-payment: u0 }
                                 (map-get? beneficiary-earnings { recipient: recipient }))))
            (map-set beneficiary-earnings
              { recipient: recipient }
              {
                total-earned: (+ (get total-earned current-earnings) payment-amount),
                payment-count: (+ (get payment-count current-earnings) u1),
                last-payment: stacks-block-height
              })
            acc))
      acc))
)

(define-private (get-single-beneficiary (index uint) (patent-id (string-ascii 64)))
  (default-to 
    { recipient: contract-owner, percentage: u0, name: u"Unknown", role: "unknown" }
    (map-get? royalty-beneficiaries { patent-id: patent-id, beneficiary-index: index }))
)

(define-private (generate-indices (count uint))
  (unwrap-panic (slice? (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9) u0 count))
)
