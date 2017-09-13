module Request.Helpers exposing (apiUrl)


apiUrl : String -> String
apiUrl str =
    "https://api.interview.healthforge.io:443/api/secure" ++ str
