module AlexaMessagesSpec where


import Test.Hspec(Spec, it, shouldBe, describe)
import AWS.AWSTypes.AlexaMessages
import AWS.AWSTypes.AlexaContext
import Data.Aeson
import Data.Either(isRight)

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        it "can parse a message without geographic authorization" $ do
            let d = "{\"context\":{\"Extensions\":{\"available\":{}},\"System\":{\"application\":{\"applicationId\":\"amzn1.ask.skill.5b3e16cb-660d-44da-8b0e-768caa8d22de\"},\"device\":{\"supportedInterfaces\":{},\"deviceId\":\"amzn1.ask.device.AFYC4KAHCGLJHYPRE2YGGQJI4YNQ2262L2YIPWOWGC6Z4E6QP34DDLRSK5IMURCW5ZHQMX66SQDGARUO4D2B3KVY7HVAH3IV7YQG5DYRLLRU6ACWSRWM4T2QQLYJBF2YWZSPCOFPL2DFJERALTWMT6EO3XDB7HQI3YFYW7E3WRLJYJBTWF74M\"},\"apiEndpoint\":\"https://api.eu.amazonalexa.com\",\"user\":{\"userId\":\"amzn1.ask.account.AFORRTTG5XIJE2BIJIFBRBTNB2DBX663K6ERQ6GNMNCQSMF7H6JWLK74ANAXQEBPXCYXCNKJ6DOL6LVLA3UNV7SXS26EO2DIRY762NIC2CAWJPYNKU4GFGL6CBPFB6GJXFYLT2ZDRJDDFH4FKONJ44NCHE5FTHKWPRH3X7VY7SHSGAUFSMHLWOH76ZRB2AVEFLXR7L4UDZBYP5Y\",\"permissions\":{\"scopes\":{\"alexa::devices:all:geolocation:read\":{\"status\":\"DENIED\"}}}},\"apiAccessToken\":\"eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJhdWQiOiJodHRwczovL2FwaS5hbWF6b25hbGV4YS5jb20iLCJpc3MiOiJBbGV4YVNraWxsS2l0Iiwic3ViIjoiYW16bjEuYXNrLnNraWxsLjViM2UxNmNiLTY2MGQtNDRkYS04YjBlLTc2OGNhYThkMjJkZSIsImV4cCI6MTYxMTY0NTY5NCwiaWF0IjoxNjExNjQ1Mzk0LCJuYmYiOjE2MTE2NDUzOTQsInByaXZhdGVDbGFpbXMiOnsiY29udGV4dCI6IkFBQUFBQUFBQVFCVjI1TlB4U2o3cUUyZ3JFMlR1Tit0S2dFQUFBQUFBQURkN29zN2w5M1pLUEYweTA5UE8zVjc1YllvbkxZcGdJRHF0akZaSEVUTVRBQ3ZOait1TGRJckxSUTlaRWNja3ZES01aTyswdUZuaFljOEhEa1Jjd2ZiQ09GcmppaFhLRUtHbUxFNUIzQm5lTHk3SzFPTzJ0aU9yQ0JBL045NTQ4UGRTdmZnc25MQkMrOWlQZVdFYXo5MEtKeVBVU1J0aGRIVGx6OUNBSmYxMWk4UFdScGRVSDE1bHJYVmorM1VOMHpUSmh6U3pUdVNCU0NFeWM4R1Q2K2FjWHJhenljNXFrMG02MDgrUGhxRlBMZHlXbkdVTnlQaGIvcVl1L3BuL2lqRVJ4WTdEQ0tUMG1vMEp0MjNtOFB5VGpSQXM5S3pKQ3lKOHJPWTJHNkplNkt1SjhFeEMxemZ0UU41UlFZWURrWld0ZWthMlhIUnhGY0FFQkFCZDZMaWttU0ttZng2MjlZcnhwOUtqdUJ5UE10V3cwRnhvOEtwWm5vTVJwK2FLUDI3azNFWTlmelpjR09KIiwiY29uc2VudFRva2VuIjpudWxsLCJkZXZpY2VJZCI6ImFtem4xLmFzay5kZXZpY2UuQUZZQzRLQUhDR0xKSFlQUkUyWUdHUUpJNFlOUTIyNjJMMllJUFdPV0dDNlo0RTZRUDM0RERMUlNLNUlNVVJDVzVaSFFNWDY2U1FER0FSVU80RDJCM0tWWTdIVkFIM0lWN1lRRzVEWVJMTFJVNkFDV1NSV000VDJRUUxZSkJGMllXWlNQQ09GUEwyREZKRVJBTFRXTVQ2RU8zWERCN0hRSTNZRllXN0UzV1JMSllKQlRXRjc0TSIsInVzZXJJZCI6ImFtem4xLmFzay5hY2NvdW50LkFGT1JSVFRHNVhJSkUyQklKSUZCUkJUTkIyREJYNjYzSzZFUlE2R05NTkNRU01GN0g2SldMSzc0QU5BWFFFQlBYQ1lYQ05LSjZET0w2TFZMQTNVTlY3U1hTMjZFTzJESVJZNzYyTklDMkNBV0pQWU5LVTRHRkdMNkNCUEZCNkdKWEZZTFQyWkRSSkRERkg0RktPTko0NE5DSEU1RlRIS1dQUkgzWDdWWTdTSFNHQVVGU01ITFdPSDc2WlJCMkFWRUZMWFI3TDRVRFpCWVA1WSJ9fQ.RFErdnHOqG3DcX40JhGbNCNEUDyhwX94Sf98BMBkgzy83U_9zvBs1sW_3AfFz9OB_qYc5owSswySwQv5rqRshPA582C44F_1rnmHT9tta9eeXhC1KoIHyFXtsWQ5YtCRxWeZs5d5dw1UaR4gTwo3eJATSaDGDqsqMrl10XUwL_cbu7adXEu7Rjn2SXoiKdiD1YsBJMBF29NR0QLqQgAijA48tKgDpgWygEzjEdrjklR_WkHxp55C1Lgw0hqkBn2JV2PMtRAcOP6Cj9e7bOYc6YZSNmioVudhckwll07UQVX9f0SIs2fRqsNsEKDwr0TxW_QUTkcWJGR4tdjbDfE2DA\",\"unit\":{\"unitId\":\"amzn1.ask.unit.AG6CV5QW6HUW7MLLRUFR7ZVIIBJPUB4VGBNKJWPAOV2IARWLX34PJJMCH7BUUZSHTVWLVPZJ7VRYHDS63KSHI2DOJVRF6LXBZURNMVDLKKTG5Q4N2YP3X6BJABKAHR5Y3A\"}}},\"version\":\"1.0\",\"session\":{\"application\":{\"applicationId\":\"amzn1.ask.skill.5b3e16cb-660d-44da-8b0e-768caa8d22de\"},\"user\":{\"userId\":\"amzn1.ask.account.AFORRTTG5XIJE2BIJIFBRBTNB2DBX663K6ERQ6GNMNCQSMF7H6JWLK74ANAXQEBPXCYXCNKJ6DOL6LVLA3UNV7SXS26EO2DIRY762NIC2CAWJPYNKU4GFGL6CBPFB6GJXFYLT2ZDRJDDFH4FKONJ44NCHE5FTHKWPRH3X7VY7SHSGAUFSMHLWOH76ZRB2AVEFLXR7L4UDZBYP5Y\",\"permissions\":{\"scopes\":{\"alexa::devices:all:geolocation:read\":{\"status\":\"DENIED\"}}}},\"new\":true,\"sessionId\":\"amzn1.echo-api.session.2a6b38ac-b905-44b9-b99e-b81f14f87afe\"},\"request\":{\"requestId\":\"amzn1.echo-api.request.513dd21d-94e6-411f-9e59-4acebf7ea110\",\"locale\":\"it-IT\",\"intent\":{\"name\":\"RegionColorIntent\",\"confirmationStatus\":\"NONE\"},\"type\":\"IntentRequest\",\"timestamp\":\"2021-01-26T07:16:34Z\"}}"
            let result = eitherDecode d :: Either String AlexaRequest
            isRight result `shouldBe` True
            let Right message = result
            coordinate <$> (alexaGeolocation . context) message `shouldBe` Nothing
        it "can parse a message without geographic authorization but with grants" $ do
            let d = "{\"context\":{\"Extensions\":{\"available\":{}},\"System\":{\"application\":{\"applicationId\":\"amzn1.ask.skill.5b3e16cb-660d-44da-8b0e-768caa8d22de\"},\"device\":{\"supportedInterfaces\":{},\"deviceId\":\"amzn1.ask.device.AFYC4KAHCGLJHYPRE2YGGQJI4YNQ2262L2YIPWOWGC6Z4E6QP34DDLRSK5IMURCW5ZHQMX66SQDGARUO4D2B3KVY7HVAH3IV7YQG5DYRLLRU6ACWSRWM4T2QQLYJBF2YWZSPCOFPL2DFJERALTWMT6EO3XDB7HQI3YFYW7E3WRLJYJBTWF74M\"},\"apiEndpoint\":\"https://api.eu.amazonalexa.com\",\"user\":{\"userId\":\"amzn1.ask.account.AFORRTTG5XIJE2BIJIFBRBTNB2DBX663K6ERQ6GNMNCQSMF7H6JWLK74ANAXQEBPXCYXCNKJ6DOL6LVLA3UNV7SXS26EO2DIRY762NIC2CAWJPYNKU4GFGL6CBPFB6GJXFYLT2ZDRJDDFH4FKONJ44NCHE5FTHKWPRH3X7VY7SHSGAUFSMHLWOH76ZRB2AVEFLXR7L4UDZBYP5Y\",\"permissions\":{\"scopes\":{\"alexa::devices:all:geolocation:read\":{\"status\":\"GRANTED\"}},\"consentToken\":\"eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJhdWQiOiJodHRwczovL2FwaS5hbWF6b25hbGV4YS5jb20iLCJpc3MiOiJBbGV4YVNraWxsS2l0Iiwic3ViIjoiYW16bjEuYXNrLnNraWxsLjViM2UxNmNiLTY2MGQtNDRkYS04YjBlLTc2OGNhYThkMjJkZSIsImV4cCI6MTYxMTgyMzE5MywiaWF0IjoxNjExODE5NTkzLCJuYmYiOjE2MTE4MTk1OTMsInByaXZhdGVDbGFpbXMiOnsiaXNEZXByZWNhdGVkIjoidHJ1ZSIsImNvbnNlbnRUb2tlbiI6IkF0emF8SXdFQklEbkNfZDNwc3hVWEVLc080Z3NtTW5qV0xUY2N2X2hUWTBuNUVET2l1VTM4RUxoS095QzVRdTFiNHdyMWlTVVdsYmhnckVNVXpFdnI4YXh6NGJJVUVKejVRNllZSDBsa3FIMS1mXzl0SnBfRnJVZlF0RjhLeVFTLURXVGxjdnlOcHdUYmRnLWw0dTlOcmZtWmUtNzBsVVMwNGZqU1lJTWY4VXRNcWxoSzFfa0hQalFRamJmV2o3TlpXQjl2c25LUGNmNW93aTVndy1XVWNIanp3cXViRDJ2Wk1Na2l0S0J5OUVUS1E3cmJOaWlCY0tGWkpJY2QtajBLM0V3RVVaLV9yVlluN1ZuY3VqblVmSktFek9pdW4tMzlzWmJER3JRbXRtU2I4YXhOQnRZTTJRIiwiZGV2aWNlSWQiOiJhbXpuMS5hc2suZGV2aWNlLkFGWUM0S0FIQ0dMSkhZUFJFMllHR1FKSTRZTlEyMjYyTDJZSVBXT1dHQzZaNEU2UVAzNERETFJTSzVJTVVSQ1c1WkhRTVg2NlNRREdBUlVPNEQyQjNLVlk3SFZBSDNJVjdZUUc1RFlSTExSVTZBQ1dTUldNNFQyUVFMWUpCRjJZV1pTUENPRlBMMkRGSkVSQUxUV01UNkVPM1hEQjdIUUkzWUZZVzdFM1dSTEpZSkJUV0Y3NE0iLCJ1c2VySWQiOiJhbXpuMS5hc2suYWNjb3VudC5BRk9SUlRURzVYSUpFMkJJSklGQlJCVE5CMkRCWDY2M0s2RVJRNkdOTU5DUVNNRjdINkpXTEs3NEFOQVhRRUJQWENZWENOS0o2RE9MNkxWTEEzVU5WN1NYUzI2RU8yRElSWTc2Mk5JQzJDQVdKUFlOS1U0R0ZHTDZDQlBGQjZHSlhGWUxUMlpEUkpEREZINEZLT05KNDROQ0hFNUZUSEtXUFJIM1g3Vlk3U0hTR0FVRlNNSExXT0g3NlpSQjJBVkVGTFhSN0w0VURaQllQNVkifX0.TUOaJAz6hSthAWjnMFSOpPM_yVhBZibyuKX4AId2sPkzcLOp6poctB5VK7bWFTs5iYgppirtfQ1H4WnfU3JWNdG3AKJZxkJ0gjuCqsV2yRJNX1cCIP_pt6lfh4LVMHwemRS12X9zOG8XOXz6XwW4IGFZiYw9bErbuHLTEjf2RDFjA1oDWhDre_YZrMMb6TEdgAAKQUn6S0JW8zmqA1PhYTHA7N3e2w7GLmB4MUAjRXJPsrsSa6BIgQeoXY0QiGTYeVhMIK76BRjjTTOOItfouK5mDYP9kKn77VUAkK6GLvHaKT77S35c5LL_pv2Ly-OEv_pOgR6zmRtKEvmaKp7tEg\"}},\"apiAccessToken\":\"eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJhdWQiOiJodHRwczovL2FwaS5hbWF6b25hbGV4YS5jb20iLCJpc3MiOiJBbGV4YVNraWxsS2l0Iiwic3ViIjoiYW16bjEuYXNrLnNraWxsLjViM2UxNmNiLTY2MGQtNDRkYS04YjBlLTc2OGNhYThkMjJkZSIsImV4cCI6MTYxMTgxOTg5MywiaWF0IjoxNjExODE5NTkzLCJuYmYiOjE2MTE4MTk1OTMsInByaXZhdGVDbGFpbXMiOnsiY29udGV4dCI6IkFBQUFBQUFBQVFEbldsWU1RRisxdnRQbU5reWJPamNrVHdFQUFBQUFBQURVeVhOdy9ybE04dE5uOTlsSFRobjA3RVpTVVA5NDR6ekNGR1ptNmlyTkdieXhLWFUzenhSTXZScEh0T0lrN1FRdEMxSmNLZjU0L2hnSUZ4a0t0ZnZlN0pNUTJpbUMvOHRCUGhxVGE2NDBXU2NYU3ZSOGZ3YUpkRXErZXk2K2tiWU92OVVOSUJycWJnVkpmbHlnZGxubjhkYllZejUrZUFoQVBUbzcrdlI1WVBGSmpPVmtGVHdjbnUzaXAxTVhvUS8zNHpxRlhWS2JHbURKdWFEcG1aall5VG9XblRrWEwxdE1Ld25HOHA3b1dwREo1MHpWRFpMbGRjUnhYQy9paFhRcGVBQmNGcDZaVTVNTk16WTRKU2lqV2xwTlNVVnhCTm1Da01rcE0zbjd1b29Xd3JSZTdhWkgxdS8rWkFMaGV4YXp0RDBSRnVHcXZxNENhV0x0eTV3MlZGb0wyMjMvcHZ0Y3FDc0xtR3Nab1l1bDVXT3hYTW9rM3FzcG5FNThIVWhiTlRPb2dCQVZtZUxJSVRxcHQ4ZmtxMGFHWXdzVFRpN0lmKzVnajdUVWM2YnhFQmpWM25kU0NyMHMweG1CMWc9PSIsImNvbnNlbnRUb2tlbiI6IkF0emF8SXdFQklEbkNfZDNwc3hVWEVLc080Z3NtTW5qV0xUY2N2X2hUWTBuNUVET2l1VTM4RUxoS095QzVRdTFiNHdyMWlTVVdsYmhnckVNVXpFdnI4YXh6NGJJVUVKejVRNllZSDBsa3FIMS1mXzl0SnBfRnJVZlF0RjhLeVFTLURXVGxjdnlOcHdUYmRnLWw0dTlOcmZtWmUtNzBsVVMwNGZqU1lJTWY4VXRNcWxoSzFfa0hQalFRamJmV2o3TlpXQjl2c25LUGNmNW93aTVndy1XVWNIanp3cXViRDJ2Wk1Na2l0S0J5OUVUS1E3cmJOaWlCY0tGWkpJY2QtajBLM0V3RVVaLV9yVlluN1ZuY3VqblVmSktFek9pdW4tMzlzWmJER3JRbXRtU2I4YXhOQnRZTTJRIiwiZGV2aWNlSWQiOiJhbXpuMS5hc2suZGV2aWNlLkFGWUM0S0FIQ0dMSkhZUFJFMllHR1FKSTRZTlEyMjYyTDJZSVBXT1dHQzZaNEU2UVAzNERETFJTSzVJTVVSQ1c1WkhRTVg2NlNRREdBUlVPNEQyQjNLVlk3SFZBSDNJVjdZUUc1RFlSTExSVTZBQ1dTUldNNFQyUVFMWUpCRjJZV1pTUENPRlBMMkRGSkVSQUxUV01UNkVPM1hEQjdIUUkzWUZZVzdFM1dSTEpZSkJUV0Y3NE0iLCJ1c2VySWQiOiJhbXpuMS5hc2suYWNjb3VudC5BRk9SUlRURzVYSUpFMkJJSklGQlJCVE5CMkRCWDY2M0s2RVJRNkdOTU5DUVNNRjdINkpXTEs3NEFOQVhRRUJQWENZWENOS0o2RE9MNkxWTEEzVU5WN1NYUzI2RU8yRElSWTc2Mk5JQzJDQVdKUFlOS1U0R0ZHTDZDQlBGQjZHSlhGWUxUMlpEUkpEREZINEZLT05KNDROQ0hFNUZUSEtXUFJIM1g3Vlk3U0hTR0FVRlNNSExXT0g3NlpSQjJBVkVGTFhSN0w0VURaQllQNVkifX0.lOg8_l-ijMQN3obKp9dQ0RuuTi54T3pKeZqucQhDH1J_C606CYN6X-Hi-or6BCYmZeOWDRW1kbmmXdUbt-fAnT0jlifjsvmvCVO0_cGW4sOxFy9nXDyaEKa35zqKQ6Ns2l-nzIQsPu8YOqbjAUjnI6R06FBZz56z7qZ3OtOwoZMQivWiCHmjAQISBguFtFYXSzfZw7KJ0a1ld5TwH3KtaYLse5c_JAfhOhxpignMTLZEs1j81h40HWxOvxH3uUC52mXjCuTyuPrHVdQabRHptJfRcKBUaPY2gZRioCEiZJXTk7Mpc4zOl9GMruhg85wbRuwwI33lmnyPE2y6aFCgQw\",\"unit\":{\"unitId\":\"amzn1.ask.unit.AG6CV5QW6HUW7MLLRUFR7ZVIIBJPUB4VGBNKJWPAOV2IARWLX34PJJMCH7BUUZSHTVWLVPZJ7VRYHDS63KSHI2DOJVRF6LXBZURNMVDLKKTG5Q4N2YP3X6BJABKAHR5Y3A\"}}},\"version\":\"1.0\",\"session\":{\"application\":{\"applicationId\":\"amzn1.ask.skill.5b3e16cb-660d-44da-8b0e-768caa8d22de\"},\"user\":{\"userId\":\"amzn1.ask.account.AFORRTTG5XIJE2BIJIFBRBTNB2DBX663K6ERQ6GNMNCQSMF7H6JWLK74ANAXQEBPXCYXCNKJ6DOL6LVLA3UNV7SXS26EO2DIRY762NIC2CAWJPYNKU4GFGL6CBPFB6GJXFYLT2ZDRJDDFH4FKONJ44NCHE5FTHKWPRH3X7VY7SHSGAUFSMHLWOH76ZRB2AVEFLXR7L4UDZBYP5Y\",\"permissions\":{\"scopes\":{\"alexa::devices:all:geolocation:read\":{\"status\":\"GRANTED\"}},\"consentToken\":\"eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJhdWQiOiJodHRwczovL2FwaS5hbWF6b25hbGV4YS5jb20iLCJpc3MiOiJBbGV4YVNraWxsS2l0Iiwic3ViIjoiYW16bjEuYXNrLnNraWxsLjViM2UxNmNiLTY2MGQtNDRkYS04YjBlLTc2OGNhYThkMjJkZSIsImV4cCI6MTYxMTgyMzE5MywiaWF0IjoxNjExODE5NTkzLCJuYmYiOjE2MTE4MTk1OTMsInByaXZhdGVDbGFpbXMiOnsiaXNEZXByZWNhdGVkIjoidHJ1ZSIsImNvbnNlbnRUb2tlbiI6IkF0emF8SXdFQklEbkNfZDNwc3hVWEVLc080Z3NtTW5qV0xUY2N2X2hUWTBuNUVET2l1VTM4RUxoS095QzVRdTFiNHdyMWlTVVdsYmhnckVNVXpFdnI4YXh6NGJJVUVKejVRNllZSDBsa3FIMS1mXzl0SnBfRnJVZlF0RjhLeVFTLURXVGxjdnlOcHdUYmRnLWw0dTlOcmZtWmUtNzBsVVMwNGZqU1lJTWY4VXRNcWxoSzFfa0hQalFRamJmV2o3TlpXQjl2c25LUGNmNW93aTVndy1XVWNIanp3cXViRDJ2Wk1Na2l0S0J5OUVUS1E3cmJOaWlCY0tGWkpJY2QtajBLM0V3RVVaLV9yVlluN1ZuY3VqblVmSktFek9pdW4tMzlzWmJER3JRbXRtU2I4YXhOQnRZTTJRIiwiZGV2aWNlSWQiOiJhbXpuMS5hc2suZGV2aWNlLkFGWUM0S0FIQ0dMSkhZUFJFMllHR1FKSTRZTlEyMjYyTDJZSVBXT1dHQzZaNEU2UVAzNERETFJTSzVJTVVSQ1c1WkhRTVg2NlNRREdBUlVPNEQyQjNLVlk3SFZBSDNJVjdZUUc1RFlSTExSVTZBQ1dTUldNNFQyUVFMWUpCRjJZV1pTUENPRlBMMkRGSkVSQUxUV01UNkVPM1hEQjdIUUkzWUZZVzdFM1dSTEpZSkJUV0Y3NE0iLCJ1c2VySWQiOiJhbXpuMS5hc2suYWNjb3VudC5BRk9SUlRURzVYSUpFMkJJSklGQlJCVE5CMkRCWDY2M0s2RVJRNkdOTU5DUVNNRjdINkpXTEs3NEFOQVhRRUJQWENZWENOS0o2RE9MNkxWTEEzVU5WN1NYUzI2RU8yRElSWTc2Mk5JQzJDQVdKUFlOS1U0R0ZHTDZDQlBGQjZHSlhGWUxUMlpEUkpEREZINEZLT05KNDROQ0hFNUZUSEtXUFJIM1g3Vlk3U0hTR0FVRlNNSExXT0g3NlpSQjJBVkVGTFhSN0w0VURaQllQNVkifX0.TUOaJAz6hSthAWjnMFSOpPM_yVhBZibyuKX4AId2sPkzcLOp6poctB5VK7bWFTs5iYgppirtfQ1H4WnfU3JWNdG3AKJZxkJ0gjuCqsV2yRJNX1cCIP_pt6lfh4LVMHwemRS12X9zOG8XOXz6XwW4IGFZiYw9bErbuHLTEjf2RDFjA1oDWhDre_YZrMMb6TEdgAAKQUn6S0JW8zmqA1PhYTHA7N3e2w7GLmB4MUAjRXJPsrsSa6BIgQeoXY0QiGTYeVhMIK76BRjjTTOOItfouK5mDYP9kKn77VUAkK6GLvHaKT77S35c5LL_pv2Ly-OEv_pOgR6zmRtKEvmaKp7tEg\"}},\"new\":true,\"sessionId\":\"amzn1.echo-api.session.6a9a3cca-0716-4f64-9b42-ead4571e7acc\"},\"request\":{\"requestId\":\"amzn1.echo-api.request.9b0d9407-6138-431e-88f6-85f06168725d\",\"locale\":\"it-IT\",\"intent\":{\"name\":\"RegionColorIntent\",\"confirmationStatus\":\"NONE\"},\"type\":\"IntentRequest\",\"timestamp\":\"2021-01-28T07:39:53Z\"}}"
            let result = eitherDecode d :: Either String AlexaRequest
            isRight result `shouldBe` True
            let Right message = result
            coordinate <$> (alexaGeolocation . context) message `shouldBe` Nothing
        it "can parse a message without any authorization" $ do
            let d = "{\"context\":{\"Extensions\":{\"available\":{}},\"System\":{\"application\":{\"applicationId\":\"amzn1.ask.skill.5b3e16cb-660d-44da-8b0e-768caa8d22de\"},\"device\":{\"supportedInterfaces\":{},\"deviceId\":\"amzn1.ask.device.AFYC4KAHCGLJHYPRE2YGGQJI4YNQ2262L2YIPWOWGC6Z4E6QP34DDLRSK5IMURCW5ZHQMX66SQDGARUO4D2B3KVY7HVAH3IV7YQG5DYRLLRU6ACWSRWM4T2QQLYJBF2YWZSPCOFPL2DFJERALTWMT6EO3XDB7HQI3YFYW7E3WRLJYJBTWF74M\"},\"apiEndpoint\":\"https://api.eu.amazonalexa.com\",\"user\":{\"userId\":\"amzn1.ask.account.AFORRTTG5XIJE2BIJIFBRBTNB2DBX663K6ERQ6GNMNCQSMF7H6JWLK74ANAXQEBPXCYXCNKJ6DOL6LVLA3UNV7SXS26EO2DIRY762NIC2CAWJPYNKU4GFGL6CBPFB6GJXFYLT2ZDRJDDFH4FKONJ44NCHE5FTHKWPRH3X7VY7SHSGAUFSMHLWOH76ZRB2AVEFLXR7L4UDZBYP5Y\",\"permissions\":{\"scopes\":{\"alexa::devices:all:geolocation:read\":{\"status\":\"DENIED\"}}}},\"apiAccessToken\":\"eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImtpZCI6IjEifQ.eyJhdWQiOiJodHRwczovL2FwaS5hbWF6b25hbGV4YS5jb20iLCJpc3MiOiJBbGV4YVNraWxsS2l0Iiwic3ViIjoiYW16bjEuYXNrLnNraWxsLjViM2UxNmNiLTY2MGQtNDRkYS04YjBlLTc2OGNhYThkMjJkZSIsImV4cCI6MTYxMTkwMjUxNiwiaWF0IjoxNjExOTAyMjE2LCJuYmYiOjE2MTE5MDIyMTYsInByaXZhdGVDbGFpbXMiOnsiY29udGV4dCI6IkFBQUFBQUFBQVFCcFZNTlhPVzQ0VXdNNTFWYXBvMHIzS2dFQUFBQUFBQUR4cnBQanFTckhZYytvUW9oUDBSZ3lvd3YzdDVRUEZyZEdrWjFFMzZwSmZBZmhwNXoxQ1ZJUjRkMlROOUY0NlFpS3g4VFVHcjg3Mk9HSHZ3bUtIb2pkeW9kYXVuNUMrZ0RpVGtUemZjdmJMS1B0SHY3QW1zYWcxemNSNksxRzA4ZUpHNHFEQ3QrOFVpa0dNWWQ1T2MxSFpyc0pzUnlWc2Vvd3dQN1BqMDYzblRjQ0R0OUYrR2RkZlE2WGd5aEJPd3lpMWxsYWVDWlEwRXlHN21Oc29wR3VJdHhYdlROek9pK0x0OE9mNGVwdStHbWtCMCtDNTBaZENGa3B6TTBsL1FHRVBKeWJxcnM0UVpwMURCc2l2K1QwdHNEQiswYTFhUFV6UzRXbTlUM3ZBTE9VenpaZGRMQlBvR09ma1dtWVRLUytvUWgzUjdhcVVwRjFma1hsZmp0ekI5enZPWTZQdUh4RUtOdk5rWFdFRXc2NzNrZWxvanNEWVVCTmprNmxibmVQeUI2ZkR1RDNIbFZ6IiwiY29uc2VudFRva2VuIjpudWxsLCJkZXZpY2VJZCI6ImFtem4xLmFzay5kZXZpY2UuQUZZQzRLQUhDR0xKSFlQUkUyWUdHUUpJNFlOUTIyNjJMMllJUFdPV0dDNlo0RTZRUDM0RERMUlNLNUlNVVJDVzVaSFFNWDY2U1FER0FSVU80RDJCM0tWWTdIVkFIM0lWN1lRRzVEWVJMTFJVNkFDV1NSV000VDJRUUxZSkJGMllXWlNQQ09GUEwyREZKRVJBTFRXTVQ2RU8zWERCN0hRSTNZRllXN0UzV1JMSllKQlRXRjc0TSIsInVzZXJJZCI6ImFtem4xLmFzay5hY2NvdW50LkFGT1JSVFRHNVhJSkUyQklKSUZCUkJUTkIyREJYNjYzSzZFUlE2R05NTkNRU01GN0g2SldMSzc0QU5BWFFFQlBYQ1lYQ05LSjZET0w2TFZMQTNVTlY3U1hTMjZFTzJESVJZNzYyTklDMkNBV0pQWU5LVTRHRkdMNkNCUEZCNkdKWEZZTFQyWkRSSkRERkg0RktPTko0NE5DSEU1RlRIS1dQUkgzWDdWWTdTSFNHQVVGU01ITFdPSDc2WlJCMkFWRUZMWFI3TDRVRFpCWVA1WSJ9fQ.G5Q6F_KO_sfCl9NaeRxTc2s5M8Wf0-jcGssSyIAc4Khy1lZ3sa6mGRRvt1XLaskXRcPzu17UmpGIQrA5RGyzU2b8QcgRQXw-DebZtxHSkOhih3YlCY_ZiTDA9UuWx-aIj5Hqcg7S7VwMnd4K-TJ7oqMuawqQkf51hNlj241Apgv_hEF69gVCFIMuChaBQX9WSm2rwUX0iFDgLmVIvmyCn5ynfB8HSjprlreNp3bZahiPmoFZ8dibAvJjuWbs1PyTdOHESy3yENgWJ5gcwvK36JkWuDkSgnmyVRXQ9_Uh3kprk20XSWzesJTAjyqyUCRzcFd2VJLAQouk3qbxCwt9BQ\",\"unit\":{\"unitId\":\"amzn1.ask.unit.AG6CV5QW6HUW7MLLRUFR7ZVIIBJPUB4VGBNKJWPAOV2IARWLX34PJJMCH7BUUZSHTVWLVPZJ7VRYHDS63KSHI2DOJVRF6LXBZURNMVDLKKTG5Q4N2YP3X6BJABKAHR5Y3A\"}}},\"version\":\"1.0\",\"session\":{\"application\":{\"applicationId\":\"amzn1.ask.skill.5b3e16cb-660d-44da-8b0e-768caa8d22de\"},\"user\":{\"userId\":\"amzn1.ask.account.AFORRTTG5XIJE2BIJIFBRBTNB2DBX663K6ERQ6GNMNCQSMF7H6JWLK74ANAXQEBPXCYXCNKJ6DOL6LVLA3UNV7SXS26EO2DIRY762NIC2CAWJPYNKU4GFGL6CBPFB6GJXFYLT2ZDRJDDFH4FKONJ44NCHE5FTHKWPRH3X7VY7SHSGAUFSMHLWOH76ZRB2AVEFLXR7L4UDZBYP5Y\",\"permissions\":{\"scopes\":{\"alexa::devices:all:geolocation:read\":{\"status\":\"DENIED\"}}}},\"new\":true,\"sessionId\":\"amzn1.echo-api.session.a5e2b0ed-92af-4b68-bc4e-0c6fbef41e3d\"},\"request\":{\"requestId\":\"amzn1.echo-api.request.7bb3cb4c-0e19-4739-96a2-9a4ae00aa749\",\"locale\":\"it-IT\",\"intent\":{\"name\":\"RegionColorIntent\",\"confirmationStatus\":\"NONE\"},\"type\":\"IntentRequest\",\"timestamp\":\"2021-01-29T06:36:56Z\"}}"
            let result = eitherDecode d :: Either String AlexaRequest
            isRight result `shouldBe` True
            let Right message = result
            coordinate <$> (alexaGeolocation . context) message `shouldBe` Nothing
    