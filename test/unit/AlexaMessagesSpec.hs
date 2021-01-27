module AlexaMessagesSpec where


import Test.Hspec(Spec, it, shouldBe, describe)
import AWS.AWSTypes.AlexaMessages
import AWS.AWSTypes.AlexaContext
import Data.Aeson
import Data.Either(isRight)

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "I can parse a message without geographic authorization" $ do
            let d = "{\"context\":{\"Extensions\":{\"available\":{}},\"System\":{\"application\":{\"applicationId\":\"amzn1.ask.skill.5b3e16cb-660d-44da-8b0e-768caa8d22de\"},\"device\":{\"supportedInterfaces\":{},\"deviceId\":\"amzn1.ask.device.AFYC4KAHCGLJHYPRE2YGGQJI4YNQ2262L2YIPWOWGC6Z4E6QP34DDLRSK5IMURCW5ZHQMX66SQDGARUO4D2B3KVY7HVAH3IV7YQG5DYRLLRU6ACWSRWM4T2QQLYJBF2YWZSPCOFPL2DFJERALTWMT6EO3XDB7HQI3YFYW7E3WRLJYJBTWF74M\"},\"apiEndpoint\":\"https://api.eu.amazonalexa.com\",\"user\":{\"userId\":\"amzn1.ask.account.AFORRTTG5XIJE2BIJIFBRBTNB2DBX663K6ERQ6GNMNCQSMF7H6JWLK74ANAXQEBPXCYXCNKJ6DOL6LVLA3UNV7SXS26EO2DIRY762NIC2CAWJPYNKU4GFGL6CBPFB6GJXFYLT2ZDRJDDFH4FKONJ44NCHE5FTHKWPRH3X7VY7SHSGAUFSMHLWOH76ZRB2AVEFLXR7L4UDZBYP5Y\",\"permissions\":{\"scopes\":{\"alexa::devices:all:geolocation:read\":{\"status\":\"DENIED\"}}}},\"apiAccessToken\":\"eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJhdWQiOiJodHRwczovL2FwaS5hbWF6b25hbGV4YS5jb20iLCJpc3MiOiJBbGV4YVNraWxsS2l0Iiwic3ViIjoiYW16bjEuYXNrLnNraWxsLjViM2UxNmNiLTY2MGQtNDRkYS04YjBlLTc2OGNhYThkMjJkZSIsImV4cCI6MTYxMTY0NTY5NCwiaWF0IjoxNjExNjQ1Mzk0LCJuYmYiOjE2MTE2NDUzOTQsInByaXZhdGVDbGFpbXMiOnsiY29udGV4dCI6IkFBQUFBQUFBQVFCVjI1TlB4U2o3cUUyZ3JFMlR1Tit0S2dFQUFBQUFBQURkN29zN2w5M1pLUEYweTA5UE8zVjc1YllvbkxZcGdJRHF0akZaSEVUTVRBQ3ZOait1TGRJckxSUTlaRWNja3ZES01aTyswdUZuaFljOEhEa1Jjd2ZiQ09GcmppaFhLRUtHbUxFNUIzQm5lTHk3SzFPTzJ0aU9yQ0JBL045NTQ4UGRTdmZnc25MQkMrOWlQZVdFYXo5MEtKeVBVU1J0aGRIVGx6OUNBSmYxMWk4UFdScGRVSDE1bHJYVmorM1VOMHpUSmh6U3pUdVNCU0NFeWM4R1Q2K2FjWHJhenljNXFrMG02MDgrUGhxRlBMZHlXbkdVTnlQaGIvcVl1L3BuL2lqRVJ4WTdEQ0tUMG1vMEp0MjNtOFB5VGpSQXM5S3pKQ3lKOHJPWTJHNkplNkt1SjhFeEMxemZ0UU41UlFZWURrWld0ZWthMlhIUnhGY0FFQkFCZDZMaWttU0ttZng2MjlZcnhwOUtqdUJ5UE10V3cwRnhvOEtwWm5vTVJwK2FLUDI3azNFWTlmelpjR09KIiwiY29uc2VudFRva2VuIjpudWxsLCJkZXZpY2VJZCI6ImFtem4xLmFzay5kZXZpY2UuQUZZQzRLQUhDR0xKSFlQUkUyWUdHUUpJNFlOUTIyNjJMMllJUFdPV0dDNlo0RTZRUDM0RERMUlNLNUlNVVJDVzVaSFFNWDY2U1FER0FSVU80RDJCM0tWWTdIVkFIM0lWN1lRRzVEWVJMTFJVNkFDV1NSV000VDJRUUxZSkJGMllXWlNQQ09GUEwyREZKRVJBTFRXTVQ2RU8zWERCN0hRSTNZRllXN0UzV1JMSllKQlRXRjc0TSIsInVzZXJJZCI6ImFtem4xLmFzay5hY2NvdW50LkFGT1JSVFRHNVhJSkUyQklKSUZCUkJUTkIyREJYNjYzSzZFUlE2R05NTkNRU01GN0g2SldMSzc0QU5BWFFFQlBYQ1lYQ05LSjZET0w2TFZMQTNVTlY3U1hTMjZFTzJESVJZNzYyTklDMkNBV0pQWU5LVTRHRkdMNkNCUEZCNkdKWEZZTFQyWkRSSkRERkg0RktPTko0NE5DSEU1RlRIS1dQUkgzWDdWWTdTSFNHQVVGU01ITFdPSDc2WlJCMkFWRUZMWFI3TDRVRFpCWVA1WSJ9fQ.RFErdnHOqG3DcX40JhGbNCNEUDyhwX94Sf98BMBkgzy83U_9zvBs1sW_3AfFz9OB_qYc5owSswySwQv5rqRshPA582C44F_1rnmHT9tta9eeXhC1KoIHyFXtsWQ5YtCRxWeZs5d5dw1UaR4gTwo3eJATSaDGDqsqMrl10XUwL_cbu7adXEu7Rjn2SXoiKdiD1YsBJMBF29NR0QLqQgAijA48tKgDpgWygEzjEdrjklR_WkHxp55C1Lgw0hqkBn2JV2PMtRAcOP6Cj9e7bOYc6YZSNmioVudhckwll07UQVX9f0SIs2fRqsNsEKDwr0TxW_QUTkcWJGR4tdjbDfE2DA\",\"unit\":{\"unitId\":\"amzn1.ask.unit.AG6CV5QW6HUW7MLLRUFR7ZVIIBJPUB4VGBNKJWPAOV2IARWLX34PJJMCH7BUUZSHTVWLVPZJ7VRYHDS63KSHI2DOJVRF6LXBZURNMVDLKKTG5Q4N2YP3X6BJABKAHR5Y3A\"}}},\"version\":\"1.0\",\"session\":{\"application\":{\"applicationId\":\"amzn1.ask.skill.5b3e16cb-660d-44da-8b0e-768caa8d22de\"},\"user\":{\"userId\":\"amzn1.ask.account.AFORRTTG5XIJE2BIJIFBRBTNB2DBX663K6ERQ6GNMNCQSMF7H6JWLK74ANAXQEBPXCYXCNKJ6DOL6LVLA3UNV7SXS26EO2DIRY762NIC2CAWJPYNKU4GFGL6CBPFB6GJXFYLT2ZDRJDDFH4FKONJ44NCHE5FTHKWPRH3X7VY7SHSGAUFSMHLWOH76ZRB2AVEFLXR7L4UDZBYP5Y\",\"permissions\":{\"scopes\":{\"alexa::devices:all:geolocation:read\":{\"status\":\"DENIED\"}}}},\"new\":true,\"sessionId\":\"amzn1.echo-api.session.2a6b38ac-b905-44b9-b99e-b81f14f87afe\"},\"request\":{\"requestId\":\"amzn1.echo-api.request.513dd21d-94e6-411f-9e59-4acebf7ea110\",\"locale\":\"it-IT\",\"intent\":{\"name\":\"RegionColorIntent\",\"confirmationStatus\":\"NONE\"},\"type\":\"IntentRequest\",\"timestamp\":\"2021-01-26T07:16:34Z\"}}"
            let result = eitherDecode d :: Either String AlexaRequest
            isRight result `shouldBe` True
            let Right message = result
            coordinate <$> (alexaGeolocation . context) message `shouldBe` Nothing
