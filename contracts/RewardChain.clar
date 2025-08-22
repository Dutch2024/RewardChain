;; RewardChain: Decentralized Loyalty & Rewards Protocol
;; Version: 1.0.0
;; A protocol for creating loyalty programs with token rewards and tier-based benefits

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-PROGRAM-NOT-FOUND (err u2))
(define-constant ERR-INVALID-REWARD-RATE (err u3))
(define-constant ERR-INVALID-DURATION (err u4))
(define-constant ERR-INVALID-NAME (err u5))
(define-constant ERR-INVALID-DESCRIPTION (err u6))
(define-constant ERR-PROGRAM-INACTIVE (err u7))
(define-constant ERR-ALREADY-ENROLLED (err u8))
(define-constant ERR-NOT-ENROLLED (err u9))
(define-constant ERR-INSUFFICIENT-FUNDS (err u10))
(define-constant ERR-REDEMPTION-NOT-READY (err u11))
(define-constant ERR-ALREADY-REDEEMED (err u12))
(define-constant ERR-INVALID-TIER (err u13))
(define-constant ERR-INVALID-PROGRAM-TYPE (err u14))
(define-constant ERR-ENROLLMENT-EXPIRED (err u15))
(define-constant ERR-INVALID-POINTS (err u16))

;; Constants
(define-constant MIN-REWARD-RATE u10) ;; 0.1% minimum reward rate
(define-constant MAX-REWARD-RATE u5000) ;; 50% maximum reward rate
(define-constant MIN-DURATION u86400) ;; 1 day minimum
(define-constant MAX-DURATION u31536000) ;; 1 year maximum
(define-constant PLATFORM-FEE-PERCENT u2) ;; 2% platform fee
(define-constant REDEMPTION-THRESHOLD u100) ;; 100 points minimum for redemption

;; Data variables
(define-data-var next-program-id uint u1)
(define-data-var next-enrollment-id uint u1)
(define-data-var platform-treasury principal tx-sender)
(define-data-var total-platform-fees uint u0)

;; Program data structure
(define-map programs
    uint
    {
        merchant: principal,
        name: (string-utf8 100),
        description: (string-utf8 500),
        tier: (string-utf8 20),
        program-type: (string-utf8 10),
        reward-rate: uint,
        stake-amount: uint,
        duration: uint,
        is-active: bool,
        total-enrollments: uint,
        total-redemptions: uint,
        created-at: uint
    })

;; Enrollment data structure
(define-map enrollments
    uint
    {
        customer: principal,
        program-id: uint,
        enrolled-at: uint,
        expires-at: uint,
        points-earned: uint,
        is-redeemed: bool,
        is-active: bool,
        stake-locked: uint
    })

;; Customer enrollments by program
(define-map customer-program-enrollments
    { customer: principal, program-id: uint }
    uint)

;; Redemption records
(define-map redemptions
    { customer: principal, program-id: uint }
    {
        redeemed-at: uint,
        final-points: uint,
        reward-hash: (string-utf8 64)
    })

;; Private validation functions
(define-private (validate-tier (tier (string-utf8 20)))
    (or 
        (is-eq tier u"Bronze")
        (is-eq tier u"Silver")
        (is-eq tier u"Gold")
        (is-eq tier u"Platinum")
        (is-eq tier u"Diamond")
        (is-eq tier u"Elite")
        (is-eq tier u"Premium")
        (is-eq tier u"VIP")
    ))

(define-private (validate-program-type (program-type (string-utf8 10)))
    (or 
        (is-eq program-type u"Retail")
        (is-eq program-type u"Dining")
        (is-eq program-type u"Travel")
        (is-eq program-type u"Gaming")
    ))

(define-private (validate-text-length (text (string-utf8 500)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    ))

(define-private (calculate-platform-fee (amount uint))
    (/ (* amount PLATFORM-FEE-PERCENT) u100))

(define-private (calculate-merchant-amount (amount uint))
    (- amount (calculate-platform-fee amount)))

(define-private (validate-stake-amount (stake-amount uint))
    (and (>= stake-amount u0) (<= stake-amount u10000000000))) ;; Max 10k STX stake

(define-private (validate-reward-hash (reward-hash (string-utf8 64)))
    (and (>= (len reward-hash) u32) (<= (len reward-hash) u64)))

;; Public functions

;; Create a new loyalty program
(define-public (create-program 
    (name (string-utf8 100))
    (description (string-utf8 500))
    (tier (string-utf8 20))
    (program-type (string-utf8 10))
    (reward-rate uint)
    (stake-amount uint)
    (duration uint))
    (let
        (
            (program-id (var-get next-program-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate inputs
        (asserts! (validate-text-length name u3 u100) ERR-INVALID-NAME)
        (asserts! (validate-text-length description u10 u500) ERR-INVALID-DESCRIPTION)
        (asserts! (validate-tier tier) ERR-INVALID-TIER)
        (asserts! (validate-program-type program-type) ERR-INVALID-PROGRAM-TYPE)
        (asserts! (and (>= reward-rate MIN-REWARD-RATE) (<= reward-rate MAX-REWARD-RATE)) ERR-INVALID-REWARD-RATE)
        (asserts! (and (>= duration MIN-DURATION) (<= duration MAX-DURATION)) ERR-INVALID-DURATION)
        (asserts! (validate-stake-amount stake-amount) ERR-INVALID-REWARD-RATE)
        
        ;; Create program
        (map-set programs program-id {
            merchant: tx-sender,
            name: name,
            description: description,
            tier: tier,
            program-type: program-type,
            reward-rate: reward-rate,
            stake-amount: stake-amount,
            duration: duration,
            is-active: true,
            total-enrollments: u0,
            total-redemptions: u0,
            created-at: current-time
        })
        
        (var-set next-program-id (+ program-id u1))
        (ok program-id)
    ))

;; Enroll in loyalty program with stake
(define-public (enroll-in-program (program-id uint))
    (let
        (
            (program (unwrap! (map-get? programs program-id) ERR-PROGRAM-NOT-FOUND))
            (enrollment-id (var-get next-enrollment-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (expires-at (+ current-time (get duration program)))
            (total-cost (+ (get reward-rate program) (get stake-amount program)))
            (platform-fee (calculate-platform-fee (get reward-rate program)))
            (merchant-amount (calculate-merchant-amount (get reward-rate program)))
        )
        ;; Validate program is active
        (asserts! (get is-active program) ERR-PROGRAM-INACTIVE)
        
        ;; Check if already enrolled
        (asserts! (is-none (map-get? customer-program-enrollments { customer: tx-sender, program-id: program-id })) ERR-ALREADY-ENROLLED)
        
        ;; Transfer reward pool to merchant and platform fee
        (try! (stx-transfer? merchant-amount tx-sender (get merchant program)))
        (try! (stx-transfer? platform-fee tx-sender (var-get platform-treasury)))
        
        ;; Lock stake amount (simulated by requiring balance)
        (asserts! (>= (stx-get-balance tx-sender) (get stake-amount program)) ERR-INSUFFICIENT-FUNDS)
        
        ;; Create enrollment
        (map-set enrollments enrollment-id {
            customer: tx-sender,
            program-id: program-id,
            enrolled-at: current-time,
            expires-at: expires-at,
            points-earned: u0,
            is-redeemed: false,
            is-active: true,
            stake-locked: (get stake-amount program)
        })
        
        ;; Map customer to enrollment
        (map-set customer-program-enrollments { customer: tx-sender, program-id: program-id } enrollment-id)
        
        ;; Update program stats
        (map-set programs program-id (merge program { total-enrollments: (+ (get total-enrollments program) u1) }))
        
        ;; Update platform fees
        (var-set total-platform-fees (+ (var-get total-platform-fees) platform-fee))
        (var-set next-enrollment-id (+ enrollment-id u1))
        
        (ok enrollment-id)
    ))

;; Update points earned
(define-public (update-points-earned (program-id uint) (points-earned uint))
    (let
        (
            (enrollment-id (unwrap! (map-get? customer-program-enrollments { customer: tx-sender, program-id: program-id }) ERR-NOT-ENROLLED))
            (enrollment (unwrap! (map-get? enrollments enrollment-id) ERR-NOT-ENROLLED))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate enrollment is active
        (asserts! (< current-time (get expires-at enrollment)) ERR-ENROLLMENT-EXPIRED)
        (asserts! (<= points-earned u10000) ERR-INVALID-POINTS)
        (asserts! (>= points-earned (get points-earned enrollment)) ERR-INVALID-POINTS)
        
        ;; Update points earned
        (map-set enrollments enrollment-id (merge enrollment { 
            points-earned: points-earned,
            is-redeemed: (>= points-earned REDEMPTION-THRESHOLD)
        }))
        
        (ok true)
    ))

;; Redeem loyalty rewards
(define-public (redeem-rewards (program-id uint) (reward-hash (string-utf8 64)))
    (let
        (
            (enrollment-id (unwrap! (map-get? customer-program-enrollments { customer: tx-sender, program-id: program-id }) ERR-NOT-ENROLLED))
            (enrollment (unwrap! (map-get? enrollments enrollment-id) ERR-NOT-ENROLLED))
            (program (unwrap! (map-get? programs program-id) ERR-PROGRAM-NOT-FOUND))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (validated-program-id (get program-id enrollment))
            (validated-hash reward-hash)
        )
        ;; Additional validations
        (asserts! (validate-reward-hash reward-hash) ERR-INVALID-DESCRIPTION)
        (asserts! (is-eq program-id validated-program-id) ERR-PROGRAM-NOT-FOUND)
        
        ;; Validate redemption eligibility
        (asserts! (get is-redeemed enrollment) ERR-REDEMPTION-NOT-READY)
        (asserts! (>= (get points-earned enrollment) REDEMPTION-THRESHOLD) ERR-REDEMPTION-NOT-READY)
        (asserts! (is-none (map-get? redemptions { customer: tx-sender, program-id: validated-program-id })) ERR-ALREADY-REDEEMED)
        
        ;; Process redemption
        (map-set redemptions { customer: tx-sender, program-id: validated-program-id } {
            redeemed-at: current-time,
            final-points: (get points-earned enrollment),
            reward-hash: validated-hash
        })
        
        ;; Update enrollment
        (map-set enrollments enrollment-id (merge enrollment { is-active: false }))
        
        ;; Update program stats
        (map-set programs validated-program-id (merge program { total-redemptions: (+ (get total-redemptions program) u1) }))
        
        ;; Return stake to customer (simulated)
        (ok true)
    ))

;; Deactivate program (merchant only)
(define-public (deactivate-program (program-id uint))
    (let
        (
            (program (unwrap! (map-get? programs program-id) ERR-PROGRAM-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get merchant program)) ERR-NOT-AUTHORIZED)
        (map-set programs program-id (merge program { is-active: false }))
        (ok true)
    ))

;; Read-only functions
(define-read-only (get-program (program-id uint))
    (map-get? programs program-id))

(define-read-only (get-enrollment (enrollment-id uint))
    (map-get? enrollments enrollment-id))

(define-read-only (get-customer-enrollment (customer principal) (program-id uint))
    (match (map-get? customer-program-enrollments { customer: customer, program-id: program-id })
        enrollment-id (map-get? enrollments enrollment-id)
        none
    ))

(define-read-only (get-redemption (customer principal) (program-id uint))
    (map-get? redemptions { customer: customer, program-id: program-id }))

(define-read-only (is-customer-redeemed (customer principal) (program-id uint))
    (is-some (map-get? redemptions { customer: customer, program-id: program-id })))

(define-read-only (get-program-stats (program-id uint))
    (match (map-get? programs program-id)
        program {
            total-enrollments: (get total-enrollments program),
            total-redemptions: (get total-redemptions program),
            redemption-rate: (if (> (get total-enrollments program) u0)
                (/ (* (get total-redemptions program) u100) (get total-enrollments program))
                u0
            )
        }
        { total-enrollments: u0, total-redemptions: u0, redemption-rate: u0 }
    ))

(define-read-only (get-platform-stats)
    {
        total-programs: (- (var-get next-program-id) u1),
        total-enrollments: (- (var-get next-enrollment-id) u1),
        total-platform-fees: (var-get total-platform-fees),
        platform-treasury: (var-get platform-treasury)
    })

(define-read-only (calculate-program-cost (program-id uint))
    (match (map-get? programs program-id)
        program {
            reward-rate: (get reward-rate program),
            stake: (get stake-amount program),
            total: (+ (get reward-rate program) (get stake-amount program)),
            platform-fee: (calculate-platform-fee (get reward-rate program)),
            merchant-amount: (calculate-merchant-amount (get reward-rate program))
        }
        { reward-rate: u0, stake: u0, total: u0, platform-fee: u0, merchant-amount: u0 }
    ))