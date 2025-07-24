;; Biodiversity Preservation Fund - Smart Contract
;; Tokenize endangered species protection with transparent funding

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-campaign-ended (err u106))
(define-constant err-campaign-active (err u107))

;; Data Variables
(define-data-var next-campaign-id uint u1)
(define-data-var platform-fee uint u25) ;; 2.5% platform fee
(define-data-var total-funds-raised uint u0)

;; Data Maps
(define-map campaigns
  { campaign-id: uint }
  {
    creator: principal,
    species-name: (string-ascii 100),
    description: (string-ascii 500),
    target-amount: uint,
    current-amount: uint,
    end-block: uint,
    active: bool,
    verified: bool
  }
)

(define-map donations
  { campaign-id: uint, donor: principal }
  { amount: uint, block-height: uint }
)

(define-map donor-campaigns
  { donor: principal, campaign-id: uint }
  { donated: bool }
)

(define-map campaign-tokens
  { campaign-id: uint }
  { 
    token-name: (string-ascii 50),
    token-symbol: (string-ascii 10),
    total-supply: uint,
    tokens-issued: uint
  }
)

(define-map token-balances
  { campaign-id: uint, holder: principal }
  { balance: uint }
)

;; Read-only functions
(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

(define-read-only (get-donation (campaign-id uint) (donor principal))
  (map-get? donations { campaign-id: campaign-id, donor: donor })
)

(define-read-only (get-token-balance (campaign-id uint) (holder principal))
  (default-to u0 (get balance (map-get? token-balances { campaign-id: campaign-id, holder: holder })))
)

(define-read-only (get-campaign-tokens (campaign-id uint))
  (map-get? campaign-tokens { campaign-id: campaign-id })
)

(define-read-only (get-platform-stats)
  {
    total-campaigns: (- (var-get next-campaign-id) u1),
    total-funds-raised: (var-get total-funds-raised),
    platform-fee: (var-get platform-fee)
  }
)

(define-read-only (is-campaign-active (campaign-id uint))
  (match (get-campaign campaign-id)
    campaign (and (get active campaign) (< block-height (get end-block campaign)))
    false
  )
)

;; Private functions
(define-private (calculate-tokens (amount uint) (token-rate uint))
  (/ (* amount u1000) token-rate) ;; Token rate in micro-units
)

(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee)) u1000)
)

;; Public functions
(define-public (create-campaign 
  (species-name (string-ascii 100))
  (description (string-ascii 500))
  (target-amount uint)
  (duration-blocks uint)
  (token-name (string-ascii 50))
  (token-symbol (string-ascii 10))
)
  (let 
    (
      (campaign-id (var-get next-campaign-id))
      (end-block (+ block-height duration-blocks))
    )
    (asserts! (> target-amount u0) err-invalid-amount)
    (asserts! (> duration-blocks u0) err-invalid-amount)
    
    ;; Create campaign
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        creator: tx-sender,
        species-name: species-name,
        description: description,
        target-amount: target-amount,
        current-amount: u0,
        end-block: end-block,
        active: true,
        verified: false
      }
    )
    
    ;; Create campaign tokens
    (map-set campaign-tokens
      { campaign-id: campaign-id }
      {
        token-name: token-name,
        token-symbol: token-symbol,
        total-supply: (* target-amount u10), ;; 10 tokens per STX
        tokens-issued: u0
      }
    )
    
    ;; Increment campaign ID
    (var-set next-campaign-id (+ campaign-id u1))
    
    (ok campaign-id)
  )
)

(define-public (donate-to-campaign (campaign-id uint) (amount uint))
  (let 
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (existing-donation (get-donation campaign-id tx-sender))
      (tokens-to-mint (calculate-tokens amount u100)) ;; 1 STX = 10 tokens
      (platform-fee (calculate-platform-fee amount))
      (net-amount (- amount platform-fee))
    )
    (asserts! (is-campaign-active campaign-id) err-campaign-ended)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update donation record
    (match existing-donation
      prev-donation 
        (map-set donations
          { campaign-id: campaign-id, donor: tx-sender }
          { 
            amount: (+ (get amount prev-donation) amount),
            block-height: block-height
          }
        )
      (map-set donations
        { campaign-id: campaign-id, donor: tx-sender }
        { amount: amount, block-height: block-height }
      )
    )
    
    ;; Update campaign amount
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { current-amount: (+ (get current-amount campaign) net-amount) })
    )
    
    ;; Mint tokens to donor
    (let 
      (
        (current-balance (get-token-balance campaign-id tx-sender))
        (token-info (unwrap! (get-campaign-tokens campaign-id) err-not-found))
      )
      (map-set token-balances
        { campaign-id: campaign-id, holder: tx-sender }
        { balance: (+ current-balance tokens-to-mint) }
      )
      
      ;; Update tokens issued
      (map-set campaign-tokens
        { campaign-id: campaign-id }
        (merge token-info { tokens-issued: (+ (get tokens-issued token-info) tokens-to-mint) })
      )
    )
    
    ;; Mark donor participation
    (map-set donor-campaigns
      { donor: tx-sender, campaign-id: campaign-id }
      { donated: true }
    )
    
    ;; Update total funds raised
    (var-set total-funds-raised (+ (var-get total-funds-raised) net-amount))
    
    (ok { donated: net-amount, tokens-received: tokens-to-mint })
  )
)

(define-public (verify-campaign (campaign-id uint))
  (let 
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { verified: true })
    )
    
    (ok true)
  )
)

(define-public (withdraw-funds (campaign-id uint))
  (let 
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
      (withdrawal-amount (get current-amount campaign))
    )
    (asserts! (is-eq tx-sender (get creator campaign)) err-unauthorized)
    (asserts! (>= block-height (get end-block campaign)) err-campaign-active)
    (asserts! (> withdrawal-amount u0) err-insufficient-funds)
    
    ;; Transfer funds to campaign creator
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender (get creator campaign))))
    
    ;; Mark campaign as inactive
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { active: false, current-amount: u0 })
    )
    
    (ok withdrawal-amount)
  )
)

(define-public (emergency-pause-campaign (campaign-id uint))
  (let 
    (
      (campaign (unwrap! (get-campaign campaign-id) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { active: false })
    )
    
    (ok true)
  )
)

(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u100) err-invalid-amount) ;; Max 10% fee
    
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-public (transfer-tokens (campaign-id uint) (recipient principal) (amount uint))
  (let 
    (
      (sender-balance (get-token-balance campaign-id tx-sender))
      (recipient-balance (get-token-balance campaign-id recipient))
    )
    (asserts! (>= sender-balance amount) err-insufficient-funds)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Update sender balance
    (map-set token-balances
      { campaign-id: campaign-id, holder: tx-sender }
      { balance: (- sender-balance amount) }
    )
    
    ;; Update recipient balance
    (map-set token-balances
      { campaign-id: campaign-id, holder: recipient }
      { balance: (+ recipient-balance amount) }
    )
    
    (ok true)
  )
)

;; Initialize contract
(begin
  (var-set next-campaign-id u1)
  (var-set platform-fee u25)
  (var-set total-funds-raised u0)
)