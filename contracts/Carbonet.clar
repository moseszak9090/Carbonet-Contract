(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-PROJECT-NOT-FOUND (err u103))
(define-constant ERR-TOKEN-NOT-FOUND (err u104))
(define-constant ERR-ALREADY-RETIRED (err u105))
(define-constant ERR-INVALID-PROJECT (err u106))
(define-constant ERR-TRANSFER-FAILED (err u107))
(define-constant ERR-UNAUTHORIZED-VERIFIER (err u108))
(define-constant ERR-PROJECT-ALREADY-EXISTS (err u109))
(define-constant ERR-AUCTION-NOT-FOUND (err u110))
(define-constant ERR-AUCTION-EXPIRED (err u111))
(define-constant ERR-AUCTION-NOT-EXPIRED (err u112))
(define-constant ERR-BID-TOO-LOW (err u113))
(define-constant ERR-AUCTION-ALREADY-FINALIZED (err u114))
(define-constant ERR-NO-BIDS (err u115))
(define-constant ERR-INVALID-DURATION (err u116))
(define-constant ERR-AUCTION-ACTIVE (err u117))

(define-data-var contract-owner principal tx-sender)
(define-data-var next-project-id uint u1)
(define-data-var next-token-id uint u1)
(define-data-var total-supply uint u0)
(define-data-var total-retired uint u0)
(define-data-var next-auction-id uint u1)

(define-map authorized-verifiers principal bool)
(define-map carbon-projects 
    uint 
    {
        project-name: (string-ascii 100),
        location: (string-ascii 100),
        methodology: (string-ascii 50),
        vintage-year: uint,
        developer: principal,
        verifier: principal,
        total-credits: uint,
        issued-credits: uint,
        price-per-credit: uint,
        verified: bool,
        active: bool
    }
)

(define-map carbon-tokens
    uint
    {
        project-id: uint,
        owner: principal,
        amount: uint,
        retired: bool,
        retirement-date: (optional uint),
        retirement-reason: (optional (string-ascii 100))
    }
)

(define-map user-balances {user: principal, project-id: uint} uint)
(define-map user-total-balance principal uint)
(define-map user-retired-balance principal uint)
(define-map project-token-supply uint uint)

(define-map carbon-auctions
    uint
    {
        seller: principal,
        project-id: uint,
        amount: uint,
        start-price: uint,
        reserve-price: uint,
        price-decrease-rate: uint,
        start-block: uint,
        duration-blocks: uint,
        finalized: bool,
        winner: (optional principal),
        final-price: (optional uint),
        active: bool
    }
)

(define-map auction-bids
    {auction-id: uint, bidder: principal}
    {
        amount: uint,
        block-height: uint,
        price-at-bid: uint
    }
)

(define-map user-auction-count principal uint)
(define-map auction-bid-count uint uint)

(define-read-only (get-contract-owner)
    (var-get contract-owner)
)

(define-read-only (get-total-supply)
    (var-get total-supply)
)

(define-read-only (get-total-retired)
    (var-get total-retired)
)

(define-read-only (get-project-info (project-id uint))
    (map-get? carbon-projects project-id)
)

(define-read-only (get-token-info (token-id uint))
    (map-get? carbon-tokens token-id)
)

(define-read-only (get-user-balance (user principal) (project-id uint))
    (default-to u0 (map-get? user-balances {user: user, project-id: project-id}))
)

(define-read-only (get-user-total-balance (user principal))
    (default-to u0 (map-get? user-total-balance user))
)

(define-read-only (get-user-retired-balance (user principal))
    (default-to u0 (map-get? user-retired-balance user))
)

(define-read-only (is-authorized-verifier (verifier principal))
    (default-to false (map-get? authorized-verifiers verifier))
)

(define-read-only (get-project-token-supply (project-id uint))
    (default-to u0 (map-get? project-token-supply project-id))
)

(define-public (add-authorized-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-set authorized-verifiers verifier true))
    )
)

(define-public (remove-authorized-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (ok (map-delete authorized-verifiers verifier))
    )
)

(define-public (create-carbon-project 
    (project-name (string-ascii 100))
    (location (string-ascii 100))
    (methodology (string-ascii 50))
    (vintage-year uint)
    (total-credits uint)
    (price-per-credit uint)
    (verifier principal))
    (let 
        (
            (project-id (var-get next-project-id))
        )
        (asserts! (> total-credits u0) ERR-INVALID-AMOUNT)
        (asserts! (> price-per-credit u0) ERR-INVALID-AMOUNT)
        (asserts! (is-none (map-get? carbon-projects project-id)) ERR-PROJECT-ALREADY-EXISTS)
        (map-set carbon-projects project-id {
            project-name: project-name,
            location: location,
            methodology: methodology,
            vintage-year: vintage-year,
            developer: tx-sender,
            verifier: verifier,
            total-credits: total-credits,
            issued-credits: u0,
            price-per-credit: price-per-credit,
            verified: false,
            active: true
        })
        (var-set next-project-id (+ project-id u1))
        (ok project-id)
    )
)

(define-public (verify-project (project-id uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get verifier project-data)) ERR-UNAUTHORIZED-VERIFIER)
        (asserts! (not (get verified project-data)) ERR-NOT-AUTHORIZED)
        (map-set carbon-projects project-id (merge project-data {verified: true}))
        (ok true)
    )
)

(define-public (mint-carbon-credits (project-id uint) (amount uint) (recipient principal))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
            (token-id (var-get next-token-id))
            (current-issued (get issued-credits project-data))
            (new-issued (+ current-issued amount))
            (current-balance (get-user-balance recipient project-id))
            (current-total-balance (get-user-total-balance recipient))
            (current-project-supply (get-project-token-supply project-id))
        )
        (asserts! (is-eq tx-sender (get developer project-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get verified project-data) ERR-INVALID-PROJECT)
        (asserts! (get active project-data) ERR-INVALID-PROJECT)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= new-issued (get total-credits project-data)) ERR-INVALID-AMOUNT)
        
        (map-set carbon-tokens token-id {
            project-id: project-id,
            owner: recipient,
            amount: amount,
            retired: false,
            retirement-date: none,
            retirement-reason: none
        })
        
        (map-set carbon-projects project-id (merge project-data {issued-credits: new-issued}))
        (map-set user-balances {user: recipient, project-id: project-id} (+ current-balance amount))
        (map-set user-total-balance recipient (+ current-total-balance amount))
        (map-set project-token-supply project-id (+ current-project-supply amount))
        (var-set next-token-id (+ token-id u1))
        (var-set total-supply (+ (var-get total-supply) amount))
        (ok token-id)
    )
)

(define-public (transfer-credits (project-id uint) (amount uint) (recipient principal))
    (let 
        (
            (sender-balance (get-user-balance tx-sender project-id))
            (recipient-balance (get-user-balance recipient project-id))
            (sender-total-balance (get-user-total-balance tx-sender))
            (recipient-total-balance (get-user-total-balance recipient))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
        (asserts! (not (is-eq tx-sender recipient)) ERR-TRANSFER-FAILED)
        
        (map-set user-balances {user: tx-sender, project-id: project-id} (- sender-balance amount))
        (map-set user-balances {user: recipient, project-id: project-id} (+ recipient-balance amount))
        (map-set user-total-balance tx-sender (- sender-total-balance amount))
        (map-set user-total-balance recipient (+ recipient-total-balance amount))
        (ok true)
    )
)

(define-public (retire-credits (project-id uint) (amount uint) (reason (string-ascii 100)))
    (let 
        (
            (user-balance (get-user-balance tx-sender project-id))
            (current-user-total-balance (get-user-total-balance tx-sender))
            (current-user-retired-balance (get-user-retired-balance tx-sender))
            (token-id (var-get next-token-id))
            (current-block burn-block-height)
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
        
        (map-set carbon-tokens token-id {
            project-id: project-id,
            owner: tx-sender,
            amount: amount,
            retired: true,
            retirement-date: (some current-block),
            retirement-reason: (some reason)
        })
        
        (map-set user-balances {user: tx-sender, project-id: project-id} (- user-balance amount))
        (map-set user-total-balance tx-sender (- current-user-total-balance amount))
        (map-set user-retired-balance tx-sender (+ current-user-retired-balance amount))
        (var-set next-token-id (+ token-id u1))
        (var-set total-supply (- (var-get total-supply) amount))
        (var-set total-retired (+ (var-get total-retired) amount))
        (ok token-id)
    )
)

(define-public (deactivate-project (project-id uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get developer project-data)) ERR-NOT-AUTHORIZED)
        (map-set carbon-projects project-id (merge project-data {active: false}))
        (ok true)
    )
)

(define-public (update-project-price (project-id uint) (new-price uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get developer project-data)) ERR-NOT-AUTHORIZED)
        (asserts! (> new-price u0) ERR-INVALID-AMOUNT)
        (map-set carbon-projects project-id (merge project-data {price-per-credit: new-price}))
        (ok true)
    )
)

(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

(define-read-only (get-project-credits-available (project-id uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (ok (- (get total-credits project-data) (get issued-credits project-data)))
    )
)

(define-read-only (calculate-purchase-cost (project-id uint) (amount uint))
    (let 
        (
            (project-data (unwrap! (map-get? carbon-projects project-id) ERR-PROJECT-NOT-FOUND))
        )
        (ok (* amount (get price-per-credit project-data)))
    )
)

(define-read-only (get-auction-info (auction-id uint))
    (map-get? carbon-auctions auction-id)
)

(define-read-only (get-auction-current-price (auction-id uint))
    (let 
        (
            (auction-data (unwrap! (map-get? carbon-auctions auction-id) ERR-AUCTION-NOT-FOUND))
            (blocks-elapsed (- burn-block-height (get start-block auction-data)))
            (price-decrease (* blocks-elapsed (get price-decrease-rate auction-data)))
            (current-price (- (get start-price auction-data) price-decrease))
        )
        (if (> current-price (get reserve-price auction-data))
            (ok current-price)
            (ok (get reserve-price auction-data))
        )
    )
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
    (map-get? auction-bids {auction-id: auction-id, bidder: bidder})
)

(define-read-only (get-user-auction-count (user principal))
    (default-to u0 (map-get? user-auction-count user))
)

(define-read-only (get-auction-bid-count (auction-id uint))
    (default-to u0 (map-get? auction-bid-count auction-id))
)

(define-read-only (is-auction-expired (auction-id uint))
    (let 
        (
            (auction-data (unwrap! (map-get? carbon-auctions auction-id) ERR-AUCTION-NOT-FOUND))
            (end-block (+ (get start-block auction-data) (get duration-blocks auction-data)))
        )
        (ok (>= burn-block-height end-block))
    )
)

(define-public (create-auction 
    (project-id uint) 
    (amount uint) 
    (start-price uint) 
    (reserve-price uint) 
    (price-decrease-rate uint) 
    (duration-blocks uint))
    (let 
        (
            (auction-id (var-get next-auction-id))
            (user-balance (get-user-balance tx-sender project-id))
            (user-auction-count-current (get-user-auction-count tx-sender))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> start-price u0) ERR-INVALID-AMOUNT)
        (asserts! (> reserve-price u0) ERR-INVALID-AMOUNT)
        (asserts! (> duration-blocks u0) ERR-INVALID-DURATION)
        (asserts! (>= start-price reserve-price) ERR-INVALID-AMOUNT)
        (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
        
        (map-set carbon-auctions auction-id {
            seller: tx-sender,
            project-id: project-id,
            amount: amount,
            start-price: start-price,
            reserve-price: reserve-price,
            price-decrease-rate: price-decrease-rate,
            start-block: burn-block-height,
            duration-blocks: duration-blocks,
            finalized: false,
            winner: none,
            final-price: none,
            active: true
        })
        
        (map-set user-balances {user: tx-sender, project-id: project-id} (- user-balance amount))
        (map-set user-total-balance tx-sender (- (get-user-total-balance tx-sender) amount))
        (map-set user-auction-count tx-sender (+ user-auction-count-current u1))
        (var-set next-auction-id (+ auction-id u1))
        (ok auction-id)
    )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let 
        (
            (auction-data (unwrap! (map-get? carbon-auctions auction-id) ERR-AUCTION-NOT-FOUND))
            (current-price (unwrap! (get-auction-current-price auction-id) ERR-AUCTION-NOT-FOUND))
            (is-expired (unwrap! (is-auction-expired auction-id) ERR-AUCTION-NOT-FOUND))
            (existing-bid (map-get? auction-bids {auction-id: auction-id, bidder: tx-sender}))
            (current-bid-count (get-auction-bid-count auction-id))
        )
        (asserts! (get active auction-data) ERR-AUCTION-NOT-FOUND)
        (asserts! (not (get finalized auction-data)) ERR-AUCTION-ALREADY-FINALIZED)
        (asserts! (not is-expired) ERR-AUCTION-EXPIRED)
        (asserts! (>= bid-amount current-price) ERR-BID-TOO-LOW)
        (asserts! (not (is-eq tx-sender (get seller auction-data))) ERR-NOT-AUTHORIZED)
        
        (match existing-bid
            prev-bid 
                (map-set auction-bids {auction-id: auction-id, bidder: tx-sender} {
                    amount: bid-amount,
                    block-height: burn-block-height,
                    price-at-bid: current-price
                })
            (begin
                (map-set auction-bids {auction-id: auction-id, bidder: tx-sender} {
                    amount: bid-amount,
                    block-height: burn-block-height,
                    price-at-bid: current-price
                })
                (map-set auction-bid-count auction-id (+ current-bid-count u1))
            )
        )
        (ok true)
    )
)

(define-public (finalize-auction (auction-id uint))
    (let 
        (
            (auction-data (unwrap! (map-get? carbon-auctions auction-id) ERR-AUCTION-NOT-FOUND))
            (is-expired (unwrap! (is-auction-expired auction-id) ERR-AUCTION-NOT-FOUND))
            (current-price (unwrap! (get-auction-current-price auction-id) ERR-AUCTION-NOT-FOUND))
            (bid-count (get-auction-bid-count auction-id))
        )
        (asserts! (get active auction-data) ERR-AUCTION-NOT-FOUND)
        (asserts! (not (get finalized auction-data)) ERR-AUCTION-ALREADY-FINALIZED)
        (asserts! is-expired ERR-AUCTION-NOT-EXPIRED)
        
        (if (> bid-count u0)
            (begin
                (map-set carbon-auctions auction-id (merge auction-data {
                    finalized: true,
                    winner: (some tx-sender),
                    final-price: (some current-price),
                    active: false
                }))
                (settle-auction-transfer auction-id tx-sender current-price)
            )
            (begin
                (map-set carbon-auctions auction-id (merge auction-data {
                    finalized: true,
                    active: false
                }))
                (return-auction-credits auction-id)
            )
        )
    )
)

(define-public (cancel-auction (auction-id uint))
    (let 
        (
            (auction-data (unwrap! (map-get? carbon-auctions auction-id) ERR-AUCTION-NOT-FOUND))
            (bid-count (get-auction-bid-count auction-id))
        )
        (asserts! (is-eq tx-sender (get seller auction-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get active auction-data) ERR-AUCTION-NOT-FOUND)
        (asserts! (not (get finalized auction-data)) ERR-AUCTION-ALREADY-FINALIZED)
        (asserts! (is-eq bid-count u0) ERR-AUCTION-ACTIVE)
        
        (map-set carbon-auctions auction-id (merge auction-data {
            active: false,
            finalized: true
        }))
        (return-auction-credits auction-id)
    )
)

(define-private (settle-auction-transfer (auction-id uint) (winner principal) (final-price uint))
    (let 
        (
            (auction-data (unwrap! (map-get? carbon-auctions auction-id) ERR-AUCTION-NOT-FOUND))
            (project-id (get project-id auction-data))
            (amount (get amount auction-data))
            (winner-balance (get-user-balance winner project-id))
            (winner-total-balance (get-user-total-balance winner))
        )
        (map-set user-balances {user: winner, project-id: project-id} (+ winner-balance amount))
        (map-set user-total-balance winner (+ winner-total-balance amount))
        (ok true)
    )
)

(define-private (return-auction-credits (auction-id uint))
    (let 
        (
            (auction-data (unwrap! (map-get? carbon-auctions auction-id) ERR-AUCTION-NOT-FOUND))
            (seller (get seller auction-data))
            (project-id (get project-id auction-data))
            (amount (get amount auction-data))
            (seller-balance (get-user-balance seller project-id))
            (seller-total-balance (get-user-total-balance seller))
        )
        (map-set user-balances {user: seller, project-id: project-id} (+ seller-balance amount))
        (map-set user-total-balance seller (+ seller-total-balance amount))
        (ok true)
    )
)
