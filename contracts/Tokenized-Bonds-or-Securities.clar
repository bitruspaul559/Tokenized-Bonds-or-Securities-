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
