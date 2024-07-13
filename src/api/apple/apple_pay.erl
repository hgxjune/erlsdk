%%------------------------------------------------------------------------------
% @doc apple_pay module
% @author hgx <hgx@live.cn>
% @copyright 2024 hgx, All rights reserved.
% @since 2024-07-11 
%%------------------------------------------------------------------------------
-module(apple_pay).
-include("debug.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([produce/1]).
-export([sandbox/1]).

%% public functions
% @doc https://developer.apple.com/documentation/appstoreservernotifications/app_store_server_notifications_v2

produce(BodyIn) ->
    ?debug("assnv2 produce"),
    catch do_assnv2(BodyIn),
    jsx:encode(#{ code => 0, status => <<"success">>, message => <<"Notification received and processed">>}).

sandbox(BodyIn) ->
    ?debug("assnv2 sandbox"),
    catch do_assnv2(BodyIn),
    jsx:encode(#{ code => 0, status => <<"success">>, message => <<"Notification received and processed">>}).


%% internal functions
do_assnv2(BodyIn) ->
    DataIn                      = jsx:decode(BodyIn),
    SignedPayload               = maps:get(<<"signedPayload">>, DataIn, undefined),

    Payload                     = unpack_payload(SignedPayload),
    NotificationType            = maps:get(<<"notificationType">>,              Payload,    undefined),
    NotificationUUID            = maps:get(<<"notificationUUID">>,              Payload,    undefined),
    SignedDate                  = maps:get(<<"signedDate">>,                    Payload,    undefined),
    Version                     = maps:get(<<"version">>,                       Payload,    undefined),
    Data                        = maps:get(<<"data">>,                          Payload,    #{}),
    AppAppleId                  = maps:get(<<"appAppleId">>,                    Data,       undefined),
    BundleId                    = maps:get(<<"bundleId">>,                      Data,       undefined),
    BundleVersion               = maps:get(<<"bundleVersion">>,                 Data,       undefined),
    Environment                 = maps:get(<<"environment">>,                   Data,       undefined),
    SignedTransactionInfo       = maps:get(<<"signedTransactionInfo">>,         Data,       undefined),
    Status                      = maps:get(<<"status">>,                        Data,       undefined),

    ?debug("Payload notificationType                : ~p", [NotificationType]),
    ?debug("Payload notificationUUID                : ~p", [NotificationUUID]),
    ?debug("Payload signedDate                      : ~p", [SignedDate]),
    ?debug("Payload version                         : ~p", [Version]),
    ?debug("Payload Data appAppleId                 : ~p", [AppAppleId]),
    ?debug("Payload Data bundleId                   : ~p", [BundleId]),
    ?debug("Payload Data bundleVersion              : ~p", [BundleVersion]),
    ?debug("Payload Data environment                : ~p", [Environment]),
    ?debug("Payload Data status                     : ~p", [Status]),

    TransactionMap              = unpack_payload(SignedTransactionInfo),
    AppAccountToken             = maps:get(<<"appAccountToken">>,               TransactionMap, undefined),
    BundleId                    = maps:get(<<"bundleId">>,                      TransactionMap, undefined),
    Currency                    = maps:get(<<"currency">>,                      TransactionMap, undefined),
    Environment                 = maps:get(<<"environment">>,                   TransactionMap, undefined),
    ExpiresDate                 = maps:get(<<"expiresDate">>,                   TransactionMap, undefined),
    InAppOwnershipType          = maps:get(<<"inAppOwnershipType">>,            TransactionMap, undefined),
    IsUpgraded                  = maps:get(<<"isUpgraded">>,                    TransactionMap, undefined),
    OfferDiscountType           = maps:get(<<"offerDiscountType">>,             TransactionMap, undefined),
    OfferIdentifier             = maps:get(<<"offerIdentifier">>,               TransactionMap, undefined),
    OfferType                   = maps:get(<<"offerType">>,                     TransactionMap, undefined),
    OriginalPurchaseDate        = maps:get(<<"originalPurchaseDate">>,          TransactionMap, undefined),
    OriginalTransactionId       = maps:get(<<"originalTransactionId">>,         TransactionMap, undefined),
    Price                       = maps:get(<<"price">>,                         TransactionMap, undefined),
    ProductId                   = maps:get(<<"productId">>,                     TransactionMap, undefined),
    PurchaseDate                = maps:get(<<"purchaseDate">>,                  TransactionMap, undefined),
    Quantity                    = maps:get(<<"quantity">>,                      TransactionMap, undefined),
    RevocationDate              = maps:get(<<"revocationDate">>,                TransactionMap, undefined),
    RevocationReason            = maps:get(<<"revocationReason">>,              TransactionMap, undefined),
    SignedDateT                 = maps:get(<<"signedDate">>,                    TransactionMap, undefined),
    Storefront                  = maps:get(<<"storefront">>,                    TransactionMap, undefined),
    StorefrontId                = maps:get(<<"storefrontId">>,                  TransactionMap, undefined),
    SubscriptionGroupIdentifier = maps:get(<<"subscriptionGroupIdentifier">>,   TransactionMap, undefined),
    TransactionId               = maps:get(<<"transactionId">>,                 TransactionMap, undefined),
    TransactionReason           = maps:get(<<"transactionReason">>,             TransactionMap, undefined),
    Type                        = maps:get(<<"type">>,                          TransactionMap, undefined),
    WebOrderLineItemId          = maps:get(<<"webOrderLineItemId">>,            TransactionMap, undefined),

    ?debug("Transaction appAccountToken             : ~p", [AppAccountToken             ]),
    ?debug("Transaction bundleId                    : ~p", [BundleId                    ]),
    ?debug("Transaction currency                    : ~p", [Currency                    ]),
    ?debug("Transaction environment                 : ~p", [Environment                 ]),
    ?debug("Transaction expiresDate                 : ~p", [ExpiresDate                 ]),
    ?debug("Transaction inAppOwnershipType          : ~p", [InAppOwnershipType          ]),
    ?debug("Transaction isUpgraded                  : ~p", [IsUpgraded                  ]),
    ?debug("Transaction offerDiscountType           : ~p", [OfferDiscountType           ]),
    ?debug("Transaction offerIdentifier             : ~p", [OfferIdentifier             ]),
    ?debug("Transaction offerType                   : ~p", [OfferType                   ]),
    ?debug("Transaction originalPurchaseDate        : ~p", [OriginalPurchaseDate        ]),
    ?debug("Transaction originalTransactionId       : ~p", [OriginalTransactionId       ]),
    ?debug("Transaction price                       : ~p", [Price                       ]),
    ?debug("Transaction productId                   : ~p", [ProductId                   ]),
    ?debug("Transaction purchaseDate                : ~p", [PurchaseDate                ]),
    ?debug("Transaction quantity                    : ~p", [Quantity                    ]),
    ?debug("Transaction revocationDate              : ~p", [RevocationDate              ]),
    ?debug("Transaction revocationReason            : ~p", [RevocationReason            ]),
    ?debug("Transaction signedDate                  : ~p", [SignedDateT                 ]),
    ?debug("Transaction storefront                  : ~p", [Storefront                  ]),
    ?debug("Transaction storefrontId                : ~p", [StorefrontId                ]),
    ?debug("Transaction subscriptionGroupIdentifier : ~p", [SubscriptionGroupIdentifier ]),
    ?debug("Transaction transactionId               : ~p", [TransactionId               ]),
    ?debug("Transaction transactionReason           : ~p", [TransactionReason           ]),
    ?debug("Transaction type                        : ~p", [Type                        ]),
    ?debug("Transaction webOrderLineItemId          : ~p", [WebOrderLineItemId          ]),

    ok.


unpack_payload(JWS) ->
    [H, P, S]   = binary:split(JWS, <<".">>, [global]),
    Header      = jsx:decode( base64:decode(H, #{padding => false, mode => urlsafe}) ),
    Payload     = jsx:decode( base64:decode(P, #{padding => false, mode => urlsafe}) ),
    Cert        = hd( maps:get(<<"x5c">>, Header, [undefined]) ),  % jwt.io 使用第一个证书
    Content     = <<H/binary, ".", P/binary>>,
    DigestType  = sha256,
    Signature   = transform_signature(S),
    PublicKey   = transform_publickey(Cert),
    Verify      = public_key:verify(Content, DigestType, Signature, PublicKey),
    if
        not Verify -> ?debug("unpack_payload JWS verify signature fail, JWS: ~p", JWS), #{};
        true -> Payload
    end.


% 参考 https://github.com/artemeff/jwt
% Transcode the ECDSA Base64-encoded signature into ASN.1/DER format
% jwt_ecdsa:signature/1
transform_signature(Sign) ->
    Signature   = base64:decode(Sign, #{padding => false, mode => urlsafe}),
    SignatureLen= byte_size(Signature),
    {RBin, SBin}= split_binary(Signature, (SignatureLen div 2)),
    R = crypto:bytes_to_integer(RBin),
    S = crypto:bytes_to_integer(SBin),
    public_key:der_encode('ECDSA-Sig-Value', #'ECDSA-Sig-Value'{ r = R, s = S }).

transform_publickey(Cert) ->
    CertFull             = <<"-----BEGIN CERTIFICATE-----\n", Cert/binary, "\n-----END CERTIFICATE-----">>,
    [{'Certificate', Certificate, not_encrypted}] = public_key:pem_decode(CertFull),
    OTPCertificate       = public_key:pkix_decode_cert( Certificate, otp ),
    % OTPCertificate       = public_key:pkix_decode_cert( base64:decode(Cert), otp ), % 效果同上，两种方式任选一
    OTPTBSCertificate    = OTPCertificate#'OTPCertificate'.tbsCertificate,
    SubjectPublicKeyInfo = OTPTBSCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo,
    ECPoint              = SubjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.subjectPublicKey,
    Algorithm            = SubjectPublicKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,
    NamedCurve           = Algorithm#'PublicKeyAlgorithm'.parameters,
    PublicKey            = {ECPoint, NamedCurve},   % type: ecdsa_public_key()
    PublicKey.





