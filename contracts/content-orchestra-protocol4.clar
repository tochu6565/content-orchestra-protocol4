;; ContentOrchestra Protocol
;; ------------------------------------------------------------
;; Error Constants and Protocol Configuration
;; ------------------------------------------------------------
(define-constant VAULT-ADMINISTRATOR tx-sender)
(define-constant UNAUTHORIZED-ACTION-ERROR (err u306))
(define-constant ADMIN-PRIVILEGE-REQUIRED (err u307))
(define-constant OPERATION-FORBIDDEN (err u308))
(define-constant ASSET-NOT-LOCATED (err u301))
(define-constant PARAMETER-OUT-OF-BOUNDS (err u304))
(define-constant AUTHENTICATION-FAILED (err u305))
(define-constant ENTRY-ALREADY-EXISTS (err u302))
(define-constant IDENTIFIER-INVALID (err u303))

;; Global state tracking variables
(define-data-var primary-asset-sequence uint u0)
(define-data-var collection-group-sequence uint u0)

;; ------------------------------------------------------------
;; Core Asset Registry Data Structure
;; ------------------------------------------------------------

;; Primary storage for all creative assets in the vault
(define-map vault-asset-registry
    {asset-identifier: uint}
    {
        work-title: (string-ascii 64),
        artist-signature: (string-ascii 32),
        ownership-principal: principal,
        content-length-metric: uint,
        blockchain-registration-height: uint,
        content-category: (string-ascii 32),
        metadata-descriptors: (list 8 (string-ascii 24))
    }
)

;; ------------------------------------------------------------
;; Collection Management Structures  
;; ------------------------------------------------------------

;; Registry for thematic asset collections
(define-map collection-registry-vault
    {collection-identifier: uint}
    {
        collection-label: (string-ascii 64),
        collection-overview: (string-ascii 256),
        collection-creator: principal,
        thematic-category: (string-ascii 32),
        creation-block-height: uint,
        modification-block-height: uint,
        total-asset-count: uint,
        collaboration-enabled: bool
    }
)

;; Tracks membership in collections
(define-map collection-membership-registry
    {collection-identifier: uint, member-principal: principal}
    {
        membership-status: bool,
        joining-block-height: uint,
        founder-privilege: bool
    }
)

;; Maps assets within specific collections
(define-map collection-asset-mapping
    {collection-identifier: uint, asset-identifier: uint}
    {
        contributing-principal: principal,
        addition-block-height: uint
    }
)

;; ------------------------------------------------------------
;; Personal Library Management Structures
;; ------------------------------------------------------------

;; Tracks user library counters for organization
(define-map user-library-tracking
    {library-owner: principal}
    {current-library-index: uint}
)

;; Personal library storage system
(define-map personal-library-vault
    {library-owner: principal, library-index: uint}
    {
        library-title: (string-ascii 64),
        library-summary: (string-ascii 128),
        creation-block-height: uint,
        update-block-height: uint,
        contained-asset-count: uint,
        public-visibility: bool
    }
)

;; Asset placement within personal libraries
(define-map library-asset-placement
    {library-owner: principal, library-index: uint, asset-identifier: uint}
    {
        placement-block-height: uint,
        arrangement-position: uint
    }
)

;; ------------------------------------------------------------
;; Access Control and Permissions Framework
;; ------------------------------------------------------------

;; Core access permission mapping
(define-map asset-access-registry
    {asset-identifier: uint, accessor-principal: principal}
    {access-authorization: bool}
)

;; Historical record of permission changes
(define-map permission-audit-trail
    {asset-identifier: uint, grantor-principal: principal, recipient-principal: principal}
    {
        grant-block-height: uint,
        revocation-block-height: uint,
        current-active-status: bool
    }
)

;; ------------------------------------------------------------
;; Community Feedback and Rating System
;; ------------------------------------------------------------

;; Individual user feedback storage
(define-map community-feedback-vault
    {asset-identifier: uint, evaluator-principal: principal}
    {
        numerical-rating: uint,
        written-commentary: (optional (string-ascii 256)),
        most-recent-update-block: uint,
        initial-feedback-block: uint
    }
)

;; Aggregated feedback statistics
(define-map feedback-analytics-summary
    {asset-identifier: uint}
    {
        total-evaluation-count: uint,
        most-recent-feedback-block: uint
    }
)

;; ------------------------------------------------------------
;; Internal Utility Functions for Validation and Processing
;; ------------------------------------------------------------

;; Verifies asset existence in vault registry
(define-private (validate-asset-existence (asset-identifier uint))
    (is-some (map-get? vault-asset-registry {asset-identifier: asset-identifier}))
)

;; Confirms ownership privileges for asset management
(define-private (confirm-ownership-rights (asset-identifier uint) (requesting-principal principal))
    (match (map-get? vault-asset-registry {asset-identifier: asset-identifier})
        asset-record (is-eq (get ownership-principal asset-record) requesting-principal)
        false
    )
)

;; Extracts content length from asset record
(define-private (extract-content-duration (asset-identifier uint))
    (default-to u0 
        (get content-length-metric 
            (map-get? vault-asset-registry {asset-identifier: asset-identifier})
        )
    )
)

;; Validates individual metadata tag format
(define-private (validate-metadata-tag (tag-string (string-ascii 24)))
    (and 
        (> (len tag-string) u0)
        (< (len tag-string) u25)
    )
)

;; Validates complete metadata tag collection
(define-private (validate-metadata-collection (tag-collection (list 8 (string-ascii 24))))
    (and
        (> (len tag-collection) u0)
        (<= (len tag-collection) u8)
        (is-eq (len (filter validate-metadata-tag tag-collection)) (len tag-collection))
    )
)

;; Retrieves most recent library index for user
(define-private (fetch-current-library-index (library-owner principal))
    (get current-library-index (default-to {current-library-index: u0} 
        (map-get? user-library-tracking {library-owner: library-owner})))
)

;; Prepares asset data for bulk processing operations
(define-private (format-asset-for-processing (asset-identifier uint))
    {asset-identifier: asset-identifier}
)

;; Batch processing helper for collection asset addition
(define-private (process-collection-asset-addition (asset-record {asset-identifier: uint}))
    (let
        ((current-asset-id (get asset-identifier asset-record)))
        (and 
            (validate-asset-existence current-asset-id)
            (map-insert collection-asset-mapping
                {collection-identifier: (var-get collection-group-sequence), asset-identifier: current-asset-id}
                {
                    contributing-principal: tx-sender,
                    addition-block-height: block-height
                }
            )
        )
    )
)

;; ------------------------------------------------------------
;; Primary Asset Management Functions
;; ------------------------------------------------------------

;; Registers new creative asset in the vault system
(define-public (register-creative-asset 
        (work-title (string-ascii 64))
        (artist-signature (string-ascii 32))
        (content-length-metric uint)
        (content-category (string-ascii 32))
        (metadata-descriptors (list 8 (string-ascii 24)))
    )
    (let
        ((next-asset-identifier (+ (var-get primary-asset-sequence) u1)))

        ;; Comprehensive input validation protocol
        (asserts! (and (> (len work-title) u0) (< (len work-title) u65)) IDENTIFIER-INVALID)
        (asserts! (and (> (len artist-signature) u0) (< (len artist-signature) u33)) IDENTIFIER-INVALID)
        (asserts! (and (> content-length-metric u0) (< content-length-metric u10000)) PARAMETER-OUT-OF-BOUNDS)
        (asserts! (and (> (len content-category) u0) (< (len content-category) u33)) IDENTIFIER-INVALID)
        (asserts! (validate-metadata-collection metadata-descriptors) IDENTIFIER-INVALID)

        ;; Asset registration in primary vault
        (map-insert vault-asset-registry
            {asset-identifier: next-asset-identifier}
            {
                work-title: work-title,
                artist-signature: artist-signature,
                ownership-principal: tx-sender,
                content-length-metric: content-length-metric,
                blockchain-registration-height: block-height,
                content-category: content-category,
                metadata-descriptors: metadata-descriptors
            }
        )

        ;; Initial access permission establishment
        (map-insert asset-access-registry
            {asset-identifier: next-asset-identifier, accessor-principal: tx-sender}
            {access-authorization: true}
        )

        ;; Update global sequence counter
        (var-set primary-asset-sequence next-asset-identifier)
        (ok next-asset-identifier)
    )
)

;; Permanently removes asset from vault registry
(define-public (delete-vault-asset (asset-identifier uint))
    (let
        ((asset-record (unwrap! (map-get? vault-asset-registry {asset-identifier: asset-identifier}) ASSET-NOT-LOCATED)))

        ;; Authorization and existence validation
        (asserts! (validate-asset-existence asset-identifier) ASSET-NOT-LOCATED)
        (asserts! (is-eq (get ownership-principal asset-record) tx-sender) AUTHENTICATION-FAILED)

        ;; Complete asset data removal
        (map-delete vault-asset-registry {asset-identifier: asset-identifier})
        (map-delete asset-access-registry {asset-identifier: asset-identifier, accessor-principal: tx-sender})
        (ok true)
    )
)

;; Transfers asset ownership to different principal
(define-public (execute-ownership-transfer (asset-identifier uint) (new-ownership-principal principal))
    (let
        ((asset-record (unwrap! (map-get? vault-asset-registry {asset-identifier: asset-identifier}) ASSET-NOT-LOCATED)))

        ;; Validation of transfer prerequisites
        (asserts! (validate-asset-existence asset-identifier) ASSET-NOT-LOCATED)
        (asserts! (is-eq (get ownership-principal asset-record) tx-sender) AUTHENTICATION-FAILED)

        ;; Ownership record modification
        (map-set vault-asset-registry
            {asset-identifier: asset-identifier}
            (merge asset-record {ownership-principal: new-ownership-principal})
        )
        (ok true)
    )
)

;; Modifies existing asset metadata and properties
(define-public (modify-asset-properties 
        (asset-identifier uint) 
        (updated-title (string-ascii 64)) 
        (updated-content-length uint) 
        (updated-category (string-ascii 32)) 
        (updated-metadata (list 8 (string-ascii 24)))
    )
    (let
        ((asset-record (unwrap! (map-get? vault-asset-registry {asset-identifier: asset-identifier}) ASSET-NOT-LOCATED)))

        ;; Comprehensive validation checks
        (asserts! (validate-asset-existence asset-identifier) ASSET-NOT-LOCATED)
        (asserts! (is-eq (get ownership-principal asset-record) tx-sender) AUTHENTICATION-FAILED)
        (asserts! (and (> (len updated-title) u0) (< (len updated-title) u65)) IDENTIFIER-INVALID)
        (asserts! (and (> updated-content-length u0) (< updated-content-length u10000)) PARAMETER-OUT-OF-BOUNDS)
        (asserts! (and (> (len updated-category) u0) (< (len updated-category) u33)) IDENTIFIER-INVALID)
        (asserts! (validate-metadata-collection updated-metadata) IDENTIFIER-INVALID)

        ;; Asset property updates
        (map-set vault-asset-registry
            {asset-identifier: asset-identifier}
            (merge asset-record {
                work-title: updated-title,
                content-length-metric: updated-content-length,
                content-category: updated-category,
                metadata-descriptors: updated-metadata
            })
        )
        (ok true)
    )
)

;; ------------------------------------------------------------
;; Personal Library Organization Functions
;; ------------------------------------------------------------

;; Adds asset to user personal library
(define-public (include-in-personal-library 
        (library-index uint)
        (asset-identifier uint)
    )
    (let
        ((library-record (unwrap! (map-get? personal-library-vault {library-owner: tx-sender, library-index: library-index}) ASSET-NOT-LOCATED))
         (asset-record (unwrap! (map-get? vault-asset-registry {asset-identifier: asset-identifier}) ASSET-NOT-LOCATED))
         (access-permissions (default-to {access-authorization: false} (map-get? asset-access-registry {asset-identifier: asset-identifier, accessor-principal: tx-sender}))))

        ;; Validation protocol for library inclusion
        (asserts! (validate-asset-existence asset-identifier) ASSET-NOT-LOCATED)
        (asserts! (or 
                    (is-eq (get ownership-principal asset-record) tx-sender)
                    (get access-authorization access-permissions)
                  ) 
                UNAUTHORIZED-ACTION-ERROR)

        ;; Duplicate entry prevention
        (asserts! (is-none (map-get? library-asset-placement {library-owner: tx-sender, library-index: library-index, asset-identifier: asset-identifier})) 
                 ENTRY-ALREADY-EXISTS)

        (ok true)
    )
)

;; ------------------------------------------------------------
;; Access Permission Management Functions
;; ------------------------------------------------------------

;; Grants access privileges to specified principal
(define-public (authorize-asset-access 
        (asset-identifier uint)
        (target-principal principal)
    )
    (let
        ((asset-record (unwrap! (map-get? vault-asset-registry {asset-identifier: asset-identifier}) ASSET-NOT-LOCATED)))

        ;; Authorization validation protocol
        (asserts! (validate-asset-existence asset-identifier) ASSET-NOT-LOCATED)
        (asserts! (is-eq (get ownership-principal asset-record) tx-sender) AUTHENTICATION-FAILED)
        (asserts! (not (is-eq tx-sender target-principal)) IDENTIFIER-INVALID)

        ;; Prevent duplicate authorization
        (asserts! (is-none (map-get? asset-access-registry {asset-identifier: asset-identifier, accessor-principal: target-principal})) 
                 ENTRY-ALREADY-EXISTS)

        ;; Access permission establishment
        (map-insert asset-access-registry
            {asset-identifier: asset-identifier, accessor-principal: target-principal}
            {access-authorization: true}
        )

        ;; Audit trail documentation
        (map-insert permission-audit-trail
            {asset-identifier: asset-identifier, grantor-principal: tx-sender, recipient-principal: target-principal}
            {
                grant-block-height: block-height,
                revocation-block-height: u0,
                current-active-status: true
            }
        )

        (ok true)
    )
)

;; Revokes previously granted access privileges
(define-public (revoke-access-authorization 
        (asset-identifier uint)
        (target-principal principal)
    )
    (let
        ((asset-record (unwrap! (map-get? vault-asset-registry {asset-identifier: asset-identifier}) ASSET-NOT-LOCATED))
         (permission-record (unwrap! (map-get? permission-audit-trail {asset-identifier: asset-identifier, grantor-principal: tx-sender, recipient-principal: target-principal}) ASSET-NOT-LOCATED)))

        ;; Revocation validation protocol
        (asserts! (validate-asset-existence asset-identifier) ASSET-NOT-LOCATED)
        (asserts! (is-eq (get ownership-principal asset-record) tx-sender) AUTHENTICATION-FAILED)
        (asserts! (get current-active-status permission-record) UNAUTHORIZED-ACTION-ERROR)

        (ok true)
    )
)

;; ------------------------------------------------------------
;; Community Feedback and Rating Functions
;; ------------------------------------------------------------

;; Submits community feedback for asset evaluation
(define-public (contribute-asset-evaluation 
        (asset-identifier uint)
        (evaluation-rating uint)
        (written-commentary (optional (string-ascii 256)))
    )
    (let
        ((asset-record (unwrap! (map-get? vault-asset-registry {asset-identifier: asset-identifier}) ASSET-NOT-LOCATED))
         (access-permissions (default-to {access-authorization: false} (map-get? asset-access-registry {asset-identifier: asset-identifier, accessor-principal: tx-sender})))
         (previous-feedback (map-get? community-feedback-vault {asset-identifier: asset-identifier, evaluator-principal: tx-sender})))

        ;; Comprehensive evaluation validation
        (asserts! (validate-asset-existence asset-identifier) ASSET-NOT-LOCATED)
        (asserts! (or 
                    (is-eq (get ownership-principal asset-record) tx-sender)
                    (get access-authorization access-permissions)
                  ) 
                UNAUTHORIZED-ACTION-ERROR)
        (asserts! (and (>= evaluation-rating u1) (<= evaluation-rating u5)) IDENTIFIER-INVALID)

        ;; Commentary length validation when provided
        (if (is-some written-commentary)
            (asserts! (and 
                        (> (len (default-to "" written-commentary)) u0) 
                        (< (len (default-to "" written-commentary)) u257)
                      ) 
                    IDENTIFIER-INVALID)
            true
        )

        ;; Feedback storage or update logic
        (if (is-some previous-feedback)
            ;; Update existing feedback record
            (map-set community-feedback-vault
                {asset-identifier: asset-identifier, evaluator-principal: tx-sender}
                {
                    numerical-rating: evaluation-rating,
                    written-commentary: written-commentary,
                    most-recent-update-block: block-height,
                    initial-feedback-block: (get initial-feedback-block (unwrap! previous-feedback ASSET-NOT-LOCATED))
                }
            )
            ;; Create new feedback record
            (map-insert community-feedback-vault
                {asset-identifier: asset-identifier, evaluator-principal: tx-sender}
                {
                    numerical-rating: evaluation-rating,
                    written-commentary: written-commentary,
                    most-recent-update-block: block-height,
                    initial-feedback-block: block-height
                }
            )
        )

        ;; Analytics summary maintenance
        (match (map-get? feedback-analytics-summary {asset-identifier: asset-identifier})
            current-analytics (map-set feedback-analytics-summary
                {asset-identifier: asset-identifier}
                (merge current-analytics {
                    total-evaluation-count: (if (is-some previous-feedback) 
                                      (get total-evaluation-count current-analytics) 
                                      (+ (get total-evaluation-count current-analytics) u1)),
                    most-recent-feedback-block: block-height
                })
            )
            (map-insert feedback-analytics-summary
                {asset-identifier: asset-identifier}
                {
                    total-evaluation-count: u1,
                    most-recent-feedback-block: block-height
                }
            )
        )

        (ok true)
    )
)

;; ------------------------------------------------------------
;; Thematic Collection Management Functions
;; ------------------------------------------------------------

;; Creates new thematic collection with initial assets
(define-public (establish-thematic-collection
        (collection-label (string-ascii 64))
        (collection-overview (string-ascii 256))
        (thematic-category (string-ascii 32))
        (founding-assets (list 20 uint))
        (enable-collaboration bool)
    )
    (let
        ((next-collection-identifier (+ (var-get collection-group-sequence) u1))
         (verified-assets (filter validate-asset-existence founding-assets)))

        ;; Collection parameter validation
        (asserts! (and (> (len collection-label) u0) (< (len collection-label) u65)) IDENTIFIER-INVALID)
        (asserts! (and (> (len collection-overview) u0) (< (len collection-overview) u257)) IDENTIFIER-INVALID)
        (asserts! (and (> (len thematic-category) u0) (< (len thematic-category) u33)) IDENTIFIER-INVALID)

        ;; Founder membership registration
        (map-insert collection-membership-registry
            {collection-identifier: next-collection-identifier, member-principal: tx-sender}
            {
                membership-status: true,
                joining-block-height: block-height,
                founder-privilege: true
            }
        )

        ;; Batch asset addition to collection
        (map process-collection-asset-addition (map format-asset-for-processing verified-assets))

        ;; Global collection counter update
        (var-set collection-group-sequence next-collection-identifier)

        (ok next-collection-identifier)
    )
)

