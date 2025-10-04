(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_BOND_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_BOND_EXPIRED (err u103))
(define-constant ERR_BOND_NOT_MATURE (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_BOND_ALREADY_EXISTS (err u106))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u107))
(define-constant ERR_TRANSFER_FAILED (err u108))

(define-constant MIN_RATING u1)
(define-constant MAX_RATING u10)
(define-constant ERR_INVALID_RATING (err u112))
(define-constant ERR_ALREADY_RATED (err u113))
(define-constant ERR_INSUFFICIENT_HOLDING_PERIOD (err u114))
(define-constant MIN_HOLDING_BLOCKS u144)

(define-constant ERR_LISTING_NOT_FOUND (err u109))
(define-constant ERR_INSUFFICIENT_LISTING (err u110))
(define-constant ERR_INVALID_PRICE (err u111))

(define-fungible-token bond-token)

(define-map bonds
  { bond-id: uint }
  {
    issuer: principal,
    name: (string-ascii 50),
    symbol: (string-ascii 10),
    total-supply: uint,
    face-value: uint,
    coupon-rate: uint,
    maturity-block: uint,
    issue-block: uint,
    is-active: bool
  }
)

(define-map bond-balances
  { bond-id: uint, holder: principal }
  { balance: uint }
)

(define-map bond-ownership
  { bond-id: uint, owner: principal, spender: principal }
  { allowance: uint }
)

(define-data-var next-bond-id uint u1)
(define-data-var total-bonds-issued uint u0)

(define-read-only (get-bond-info (bond-id uint))
  (map-get? bonds { bond-id: bond-id })
)

(define-read-only (get-bond-balance (bond-id uint) (holder principal))
  (default-to u0 (get balance (map-get? bond-balances { bond-id: bond-id, holder: holder })))
)

(define-read-only (get-bond-allowance (bond-id uint) (owner principal) (spender principal))
  (default-to u0 (get allowance (map-get? bond-ownership { bond-id: bond-id, owner: owner, spender: spender })))
)

(define-read-only (get-next-bond-id)
  (var-get next-bond-id)
)

(define-read-only (get-total-bonds-issued)
  (var-get total-bonds-issued)
)

(define-read-only (calculate-coupon-payment (bond-id uint) (holder-balance uint))
  (match (get-bond-info bond-id)
    bond-data
    (let
      (
        (face-value (get face-value bond-data))
        (coupon-rate (get coupon-rate bond-data))
        (total-supply (get total-supply bond-data))
      )
      (/ (* (* holder-balance face-value) coupon-rate) (* total-supply u10000))
    )
    u0
  )
)

(define-read-only (is-bond-mature (bond-id uint))
  (match (get-bond-info bond-id)
    bond-data
    (>= stacks-block-height (get maturity-block bond-data))
    false
  )
)

(define-public (issue-bond 
  (name (string-ascii 50))
  (symbol (string-ascii 10))
  (total-supply uint)
  (face-value uint)
  (coupon-rate uint)
  (maturity-blocks uint)
)
  (let
    (
      (bond-id (var-get next-bond-id))
      (maturity-block (+ stacks-block-height maturity-blocks))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> total-supply u0) ERR_INVALID_AMOUNT)
    (asserts! (> face-value u0) ERR_INVALID_AMOUNT)
    (asserts! (> maturity-blocks u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (get-bond-info bond-id)) ERR_BOND_ALREADY_EXISTS)
    
    (map-set bonds
      { bond-id: bond-id }
      {
        issuer: tx-sender,
        name: name,
        symbol: symbol,
        total-supply: total-supply,
        face-value: face-value,
        coupon-rate: coupon-rate,
        maturity-block: maturity-block,
        issue-block: stacks-block-height,
        is-active: true
      }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: tx-sender }
      { balance: total-supply }
    )
    
    (var-set next-bond-id (+ bond-id u1))
    (var-set total-bonds-issued (+ (var-get total-bonds-issued) u1))
    
    (ok bond-id)
  )
)

(define-public (purchase-bond (bond-id uint) (amount uint))
  (let
    (
      (bond-info (unwrap! (get-bond-info bond-id) ERR_BOND_NOT_FOUND))
      (issuer (get issuer bond-info))
      (face-value (get face-value bond-info))
      (total-supply (get total-supply bond-info))
      (issuer-balance (get-bond-balance bond-id issuer))
      (purchase-price (/ (* amount face-value) total-supply))
    )
    (asserts! (get is-active bond-info) ERR_BOND_EXPIRED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= issuer-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (< stacks-block-height (get maturity-block bond-info)) ERR_BOND_EXPIRED)
    
    (try! (stx-transfer? purchase-price tx-sender issuer))
    
    (map-set bond-balances
      { bond-id: bond-id, holder: issuer }
      { balance: (- issuer-balance amount) }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: tx-sender }
      { balance: (+ (get-bond-balance bond-id tx-sender) amount) }
    )
    
    (ok true)
  )
)

(define-public (transfer-bond (bond-id uint) (amount uint) (recipient principal))
  (let
    (
      (sender-balance (get-bond-balance bond-id tx-sender))
    )
    (asserts! (is-some (get-bond-info bond-id)) ERR_BOND_NOT_FOUND)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)
    
    (map-set bond-balances
      { bond-id: bond-id, holder: tx-sender }
      { balance: (- sender-balance amount) }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: recipient }
      { balance: (+ (get-bond-balance bond-id recipient) amount) }
    )
    
    (ok true)
  )
)

(define-public (approve-bond (bond-id uint) (spender principal) (amount uint))
  (begin
    (asserts! (is-some (get-bond-info bond-id)) ERR_BOND_NOT_FOUND)
    (map-set bond-ownership
      { bond-id: bond-id, owner: tx-sender, spender: spender }
      { allowance: amount }
    )
    (ok true)
  )
)

(define-public (transfer-from-bond (bond-id uint) (owner principal) (recipient principal) (amount uint))
  (let
    (
      (allowance (get-bond-allowance bond-id owner tx-sender))
      (owner-balance (get-bond-balance bond-id owner))
    )
    (asserts! (is-some (get-bond-info bond-id)) ERR_BOND_NOT_FOUND)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= allowance amount) ERR_UNAUTHORIZED)
    (asserts! (>= owner-balance amount) ERR_INSUFFICIENT_BALANCE)
    
    (map-set bond-ownership
      { bond-id: bond-id, owner: owner, spender: tx-sender }
      { allowance: (- allowance amount) }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: owner }
      { balance: (- owner-balance amount) }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: recipient }
      { balance: (+ (get-bond-balance bond-id recipient) amount) }
    )
    
    (ok true)
  )
)

(define-public (claim-coupon (bond-id uint))
  (let
    (
      (bond-info (unwrap! (get-bond-info bond-id) ERR_BOND_NOT_FOUND))
      (holder-balance (get-bond-balance bond-id tx-sender))
      (coupon-payment (calculate-coupon-payment bond-id holder-balance))
      (issuer (get issuer bond-info))
    )
    (asserts! (> holder-balance u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (get is-active bond-info) ERR_BOND_EXPIRED)
    (asserts! (> coupon-payment u0) ERR_INVALID_AMOUNT)
    
    (try! (as-contract (stx-transfer? coupon-payment (as-contract tx-sender) tx-sender)))
    
    (ok coupon-payment)
  )
)

(define-public (redeem-bond (bond-id uint))
  (let
    (
      (bond-info (unwrap! (get-bond-info bond-id) ERR_BOND_NOT_FOUND))
      (holder-balance (get-bond-balance bond-id tx-sender))
      (face-value (get face-value bond-info))
      (total-supply (get total-supply bond-info))
      (redemption-value (/ (* holder-balance face-value) total-supply))
      (issuer (get issuer bond-info))
    )
    (asserts! (> holder-balance u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (is-bond-mature bond-id) ERR_BOND_NOT_MATURE)
    
    (try! (as-contract (stx-transfer? redemption-value (as-contract tx-sender) tx-sender)))
    
    (map-set bond-balances
      { bond-id: bond-id, holder: tx-sender }
      { balance: u0 }
    )
    
    (ok redemption-value)
  )
)

(define-public (fund-contract)
  (let
    (
      (amount (stx-get-balance tx-sender))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (stx-transfer? amount tx-sender (as-contract tx-sender))
  )
)



(define-map marketplace-listings
  { bond-id: uint, seller: principal }
  { amount: uint, price-per-unit: uint }
)

(define-data-var marketplace-fee uint u250)

(define-read-only (get-marketplace-listing (bond-id uint) (seller principal))
  (map-get? marketplace-listings { bond-id: bond-id, seller: seller })
)

(define-read-only (get-marketplace-fee)
  (var-get marketplace-fee)
)

(define-public (list-bonds-for-sale (bond-id uint) (amount uint) (price-per-unit uint))
  (let
    (
      (seller-balance (get-bond-balance bond-id tx-sender))
    )
    (asserts! (is-some (get-bond-info bond-id)) ERR_BOND_NOT_FOUND)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-unit u0) ERR_INVALID_PRICE)
    (asserts! (>= seller-balance amount) ERR_INSUFFICIENT_BALANCE)
    
    (map-set marketplace-listings
      { bond-id: bond-id, seller: tx-sender }
      { amount: amount, price-per-unit: price-per-unit }
    )
    
    (ok true)
  )
)

(define-public (buy-bonds-from-market (bond-id uint) (seller principal) (amount uint))
  (let
    (
      (listing (unwrap! (get-marketplace-listing bond-id seller) ERR_LISTING_NOT_FOUND))
      (available-amount (get amount listing))
      (price-per-unit (get price-per-unit listing))
      (total-cost (* amount price-per-unit))
      (marketplace-fee-amount (/ (* total-cost (var-get marketplace-fee)) u10000))
      (seller-payment (- total-cost marketplace-fee-amount))
    )
    (asserts! (is-some (get-bond-info bond-id)) ERR_BOND_NOT_FOUND)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= available-amount amount) ERR_INSUFFICIENT_LISTING)
    (asserts! (>= (get-bond-balance bond-id seller) amount) ERR_INSUFFICIENT_BALANCE)
    
    (try! (stx-transfer? seller-payment tx-sender seller))
    (try! (stx-transfer? marketplace-fee-amount tx-sender (as-contract tx-sender)))
    
    (map-set bond-balances
      { bond-id: bond-id, holder: seller }
      { balance: (- (get-bond-balance bond-id seller) amount) }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: tx-sender }
      { balance: (+ (get-bond-balance bond-id tx-sender) amount) }
    )
    
    (if (> available-amount amount)
      (map-set marketplace-listings
        { bond-id: bond-id, seller: seller }
        { amount: (- available-amount amount), price-per-unit: price-per-unit }
      )
      (map-delete marketplace-listings { bond-id: bond-id, seller: seller })
    )
    
    (ok true)
  )
)

(define-public (cancel-listing (bond-id uint))
  (begin
    (asserts! (is-some (get-marketplace-listing bond-id tx-sender)) ERR_LISTING_NOT_FOUND)
    (map-delete marketplace-listings { bond-id: bond-id, seller: tx-sender })
    (ok true)
  )
)

(define-map bond-ratings
  { bond-id: uint }
  { 
    issuer-rating: uint,
    community-rating: uint,
    total-community-votes: uint,
    community-rating-sum: uint
  }
)

(define-map user-bond-ratings
  { bond-id: uint, rater: principal }
  { rating: uint, voted-at-block: uint }
)

(define-read-only (get-bond-rating (bond-id uint))
  (map-get? bond-ratings { bond-id: bond-id })
)

(define-read-only (get-user-rating (bond-id uint) (rater principal))
  (map-get? user-bond-ratings { bond-id: bond-id, rater: rater })
)

(define-read-only (calculate-composite-rating (bond-id uint))
  (match (get-bond-rating bond-id)
    rating-data
    (let
      (
        (issuer-rating (get issuer-rating rating-data))
        (community-votes (get total-community-votes rating-data))
        (community-sum (get community-rating-sum rating-data))
      )
      (if (> community-votes u0)
        (/ (+ (* issuer-rating u3) (/ community-sum community-votes)) u4)
        issuer-rating
      )
    )
    u0
  )
)

(define-public (set-issuer-rating (bond-id uint) (rating uint))
  (let
    (
      (bond-info (unwrap! (get-bond-info bond-id) ERR_BOND_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get issuer bond-info)) ERR_UNAUTHORIZED)
    (asserts! (and (>= rating MIN_RATING) (<= rating MAX_RATING)) ERR_INVALID_RATING)
    
    (map-set bond-ratings
      { bond-id: bond-id }
      (merge 
        (default-to 
          { issuer-rating: u0, community-rating: u0, total-community-votes: u0, community-rating-sum: u0 }
          (get-bond-rating bond-id)
        )
        { issuer-rating: rating }
      )
    )
    (ok true)
  )
)

(define-public (submit-community-rating (bond-id uint) (rating uint))
  (let
    (
      (bond-info (unwrap! (get-bond-info bond-id) ERR_BOND_NOT_FOUND))
      (user-balance (get-bond-balance bond-id tx-sender))
      (existing-rating (get-user-rating bond-id tx-sender))
      (current-ratings (default-to 
        { issuer-rating: u0, community-rating: u0, total-community-votes: u0, community-rating-sum: u0 }
        (get-bond-rating bond-id)
      ))
    )
    (asserts! (> user-balance u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (and (>= rating MIN_RATING) (<= rating MAX_RATING)) ERR_INVALID_RATING)
    (asserts! (is-none existing-rating) ERR_ALREADY_RATED)
    (asserts! (>= (- stacks-block-height (get issue-block bond-info)) MIN_HOLDING_BLOCKS) ERR_INSUFFICIENT_HOLDING_PERIOD)
    
    (map-set user-bond-ratings
      { bond-id: bond-id, rater: tx-sender }
      { rating: rating, voted-at-block: stacks-block-height }
    )
    
    (map-set bond-ratings
      { bond-id: bond-id }
      {
        issuer-rating: (get issuer-rating current-ratings),
        community-rating: (if (> (+ (get total-community-votes current-ratings) u1) u0)
          (/ (+ (get community-rating-sum current-ratings) rating) (+ (get total-community-votes current-ratings) u1))
          rating
        ),
        total-community-votes: (+ (get total-community-votes current-ratings) u1),
        community-rating-sum: (+ (get community-rating-sum current-ratings) rating)
      }
    )
    (ok true)
  )
)

(define-constant ERR_ORACLE_NOT_INITIALIZED (err u115))
(define-constant ERR_INSUFFICIENT_TRADE_DATA (err u116))

(define-map bond-price-oracle
  { bond-id: uint }
  { 
    last-trade-price: uint,
    last-trade-block: uint,
    trade-count: uint,
    cumulative-volume: uint,
    theoretical-price: uint
  }
)

(define-map recent-trades
  { bond-id: uint, trade-index: uint }
  { price: uint, volume: uint, block-height: uint }
)

(define-data-var max-trade-history uint u10)

(define-read-only (get-oracle-data (bond-id uint))
  (map-get? bond-price-oracle { bond-id: bond-id })
)

(define-read-only (calculate-yield-to-maturity (bond-id uint) (market-price uint))
  (match (get-bond-info bond-id)
    bond-data
    (let
      (
        (face-value (get face-value bond-data))
        (coupon-rate (get coupon-rate bond-data))
        (blocks-to-maturity (- (get maturity-block bond-data) stacks-block-height))
        (annual-coupon (* face-value (/ coupon-rate u10000)))
      )
      (if (> blocks-to-maturity u0)
        (/ (* (+ annual-coupon (/ (* (- face-value market-price) u10000) blocks-to-maturity)) u10000) market-price)
        u0
      )
    )
    u0
  )
)

(define-read-only (get-theoretical-price (bond-id uint))
  (match (get-bond-info bond-id)
    bond-data
    (let
      (
        (face-value (get face-value bond-data))
        (coupon-rate (get coupon-rate bond-data))
        (blocks-to-maturity (- (get maturity-block bond-data) stacks-block-height))
        (market-rate u500)
      )
      (if (> blocks-to-maturity u0)
        (+ 
          (/ (* face-value u10000) (+ u10000 (* market-rate (/ blocks-to-maturity u52560))))
          (/ (* (* face-value coupon-rate) blocks-to-maturity) (* u10000 (+ u10000 (* market-rate (/ blocks-to-maturity u52560)))))
        )
        face-value
      )
    )
    u0
  )
)

(define-public (update-oracle (bond-id uint) (trade-price uint) (trade-volume uint))
  (let
    (
      (current-oracle (default-to 
        { last-trade-price: u0, last-trade-block: u0, trade-count: u0, cumulative-volume: u0, theoretical-price: u0 }
        (get-oracle-data bond-id)
      ))
      (theoretical-price (get-theoretical-price bond-id))
      (new-trade-count (+ (get trade-count current-oracle) u1))
    )
    (asserts! (is-some (get-bond-info bond-id)) ERR_BOND_NOT_FOUND)
    (asserts! (> trade-price u0) ERR_INVALID_PRICE)
    (asserts! (> trade-volume u0) ERR_INVALID_AMOUNT)
    
    (map-set recent-trades
      { bond-id: bond-id, trade-index: (mod new-trade-count (var-get max-trade-history)) }
      { price: trade-price, volume: trade-volume, block-height: stacks-block-height }
    )
    
    (map-set bond-price-oracle
      { bond-id: bond-id }
      {
        last-trade-price: trade-price,
        last-trade-block: stacks-block-height,
        trade-count: new-trade-count,
        cumulative-volume: (+ (get cumulative-volume current-oracle) trade-volume),
        theoretical-price: theoretical-price
      }
    )
    (ok true)
  )
)

(define-read-only (get-simple-average-price (bond-id uint))
  (match (get-oracle-data bond-id)
    oracle-data
    (get last-trade-price oracle-data)
    u0
  )
)

(define-constant ERR_ESCROW_NOT_FOUND (err u117))
(define-constant ERR_ESCROW_ALREADY_SETTLED (err u118))
(define-constant ERR_ESCROW_NOT_SELLER (err u119))
(define-constant ERR_ESCROW_NOT_BUYER (err u120))

(define-map bond-escrows
  { escrow-id: uint }
  {
    bond-id: uint,
    seller: principal,
    buyer: (optional principal),
    bond-amount: uint,
    stx-amount: uint,
    is-settled: bool,
    created-at-block: uint
  }
)

(define-data-var next-escrow-id uint u1)

(define-read-only (get-escrow-info (escrow-id uint))
  (map-get? bond-escrows { escrow-id: escrow-id })
)

(define-read-only (get-next-escrow-id)
  (var-get next-escrow-id)
)

(define-public (create-escrow (bond-id uint) (bond-amount uint) (stx-amount uint) (buyer (optional principal)))
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (seller-balance (get-bond-balance bond-id tx-sender))
    )
    (asserts! (is-some (get-bond-info bond-id)) ERR_BOND_NOT_FOUND)
    (asserts! (> bond-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> stx-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= seller-balance bond-amount) ERR_INSUFFICIENT_BALANCE)
    
    (map-set bond-escrows
      { escrow-id: escrow-id }
      {
        bond-id: bond-id,
        seller: tx-sender,
        buyer: buyer,
        bond-amount: bond-amount,
        stx-amount: stx-amount,
        is-settled: false,
        created-at-block: stacks-block-height
      }
    )
    
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (settle-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (get-escrow-info escrow-id) ERR_ESCROW_NOT_FOUND))
      (bond-id (get bond-id escrow))
      (seller (get seller escrow))
      (specified-buyer (get buyer escrow))
      (bond-amount (get bond-amount escrow))
      (stx-amount (get stx-amount escrow))
    )
    (asserts! (not (get is-settled escrow)) ERR_ESCROW_ALREADY_SETTLED)
    (asserts! (or (is-none specified-buyer) (is-eq (some tx-sender) specified-buyer)) ERR_ESCROW_NOT_BUYER)
    (asserts! (>= (get-bond-balance bond-id seller) bond-amount) ERR_INSUFFICIENT_BALANCE)
    
    (try! (stx-transfer? stx-amount tx-sender seller))
    (unwrap! (transfer-bond-internal bond-id bond-amount seller tx-sender) ERR_TRANSFER_FAILED)
    
    (map-set bond-escrows
      { escrow-id: escrow-id }
      (merge escrow { is-settled: true })
    )
    (ok true)
  )
)

(define-private (transfer-bond-internal (bond-id uint) (amount uint) (from principal) (to principal))
  (let
    (
      (from-balance (get-bond-balance bond-id from))
    )
    (map-set bond-balances { bond-id: bond-id, holder: from } { balance: (- from-balance amount) })
    (map-set bond-balances { bond-id: bond-id, holder: to } { balance: (+ (get-bond-balance bond-id to) amount) })
    (ok true)
  )
)