# wreq-tut
Tutorial for wreq an haskell client library.

## Lens
Lens are used to set/get to/from the data. we are not going to deep dive into the lens, we will see the simple functions required to use wReq library in getting things done. `view (^.)`, `set (.~)` are the basic functions to get and set the values

> `>>> view _2 (1,"hello")`
<br />"hello"

> `>>> (1, "hello") ^. _2`
<br /> "hello"

> `>>> set _2 "hello" (1, 2)`
<br /> (1, "hello")

> `>>> (1, 2) & _2 .~ "hello"`
<br /> (1, "hello")

## wreq
wreq is a http client library to make the API calls easily using the potential of lens to modify or access Request and Response. <br />

Required imports:
``` haskell
-- ^ wreq package
import Newton.Wreq
-- ^ Accessors
import Control.Lens
```

### Simple Get Request
Here we will show how to do a simple get request and log the response by using lens.

``` haskell
resp <- get "http://httpbin.org/get"
resp ^. responseBody

RespBody:
{
  "args": {},
  "headers": {
    "Accept-Encoding": "gzip",
    "Host": "httpbin.org",
    "User-Agent": "haskell wreq-0.5.3.3",
    "X-Amzn-Trace-Id": "Root=1-6134aa81-40d492483b5824960758c3df"
  },
  "origin": "103.46.237.84",
  "url": "http://httpbin.org/get"
}
```
You can pass the query params using `getWith`

``` haskell
functions to look at:
>>> param :: Text -> Lens' Options [Text]
>>> defaults :: Options


{-# LANGUAGE OverloadedStrings #-} -- param uses Text

let opts = defaults & param "foo" .~ ["bar", "quux"]
getWith opts "http://httpbin.org/get"

RespBody:
{
  "args": {
    "foo": [
      "bar",
      "quux"
    ]
  },
  "headers": {
    "Accept-Encoding": "gzip",
    "Host": "httpbin.org",
    "User-Agent": "haskell wreq-0.5.3.3",
    "X-Amzn-Trace-Id": "Root=1-6134ac87-19afc20e5e6459581d285d83"
  },
  "origin": "103.46.237.84",
  "url": "http://httpbin.org/get?foo=bar&foo=quux"
}

```

## Post Request
Post request can be done using `post` function, where all the types to be passed as request body should have instance of [Postable](https://hackage.haskell.org/package/wreq-0.5.3.3/docs/Network-Wreq-Types.html#v:postPayload) class. In the below example we will create a type and derive `ToJSON` instance from `aeson` package and converts it to Value to satisfy `Postable` constraint.

``` haskell
data ExRequest = ExRequest
  { num :: Int
  , str :: String
  }
  deriving (Generic, ToJSON) -- Add DeriveGeneric Extension

let req = ExRequest 1 "one"
resp <- post "http://httpbin.org/post" (toJSON req)
return $ resp ^. responseBody

RespBody:
{
  "args": {},
  "data": "{\"num\":1,\"str\":\"one\"}",
  "files": {},
  "form": {},
  "headers": {
    "Accept-Encoding": "gzip",
    "Content-Length": "21",
    "Content-Type": "application/json",
    "Host": "httpbin.org",
    "User-Agent": "haskell wreq-0.5.3.3",
    "X-Amzn-Trace-Id": "Root=1-61363369-5a961a4b6bc037814506e1fc"
  },
  "json": {
    "num": 1,
    "str": "one"
  },
  "origin": "122.186.67.78",
  "url": "http://httpbin.org/post"
}
```

As seen above, we can pass the query params using the `postWith`

``` haskell
let opts = defaults & param "foo" .~ ["bar", "quux"]
let req = ExRequest 1 "one"
resp <- postWith opts "http://httpbin.org/post" (toJSON req)
return $ resp ^. responseBody

RespBody:
{
  "args": {
    "foo": [
      "bar",
      "quux"
    ]
  },
  "data": "{\"num\":1,\"str\":\"one\"}",
  "files": {},
  "form": {},
  "headers": {
    "Accept-Encoding": "gzip",
    "Content-Length": "21",
    "Content-Type": "application/json",
    "Host": "httpbin.org",
    "User-Agent": "haskell wreq-0.5.3.3",
    "X-Amzn-Trace-Id": "Root=1-6136348d-15571a6307d9638425d6a5f5"
  },
  "json": {
    "num": 1,
    "str": "one"
  },
  "origin": "122.186.67.78",
  "url": "http://httpbin.org/post?foo=bar&foo=quux"
}
```

## Headers
Post/Get uses the Options to send the Headers for each API. 

```haskell
let opts = defaults & header "Accept" .~ ["*/*"]
getWith opts "http://httpbin.org/get"


RespBody:
{
  "args": {},
  "headers": {
    "Accept-Encoding": "gzip",
    "Host": "httpbin.org",
    "User-Agent": "haskell wreq-0.5.3.3",
    "X-Amzn-Trace-Id": "Root=1-613635a7-127a7291650e57244cf8512f"
  },
  "origin": "122.186.67.78",
  "url": "http://httpbin.org/get"
}

```
Like this you can pass differnt headers using the lens set and defautls. Will see example using Auth header.

### Auth header using username, password.
```haskell
let opts = defaults & auth ?~ basicAuth "user" "pass"
getWith opts "https://httpbin.org/basic-auth/user/pass"
```

### Auth headers using Bearer Token.
```haskell
  -- generated this bearer with curl /v1/token at: https://instantwebtools.net/secured-fake-rest-api
let opts = defaults & auth ?~ oauth2Bearer "eyJraWQiOiJQZjNoZlFEYVdYM0dMekJ2ZHk3MkhzVExRWUhKbk45dldFQm1LLUItUFJvIiwiYWxnIjoiUlMyNTYifQ.eyJ2ZXIiOjEsImp0aSI6IkFULnJjbHppTGR3QUtRdUQ1X3R2Smk1ZnpKQ3BTLXV2WWFiQVE4d0pUb01YRnMub2FyY2p1czlucDJVdHE0MUY0eDYiLCJpc3MiOiJodHRwczovL2Rldi00NTc5MzEub2t0YS5jb20vb2F1dGgyL2F1c2hkNGM5NVF0RkhzZld0NHg2IiwiYXVkIjoiYXBpIiwiaWF0IjoxNjMwOTU3NzAwLCJleHAiOjE2MzA5NjEzMDAsImNpZCI6IjBvYWhkaGprdXRhR2NJSzJNNHg2IiwidWlkIjoiMDB1aGVuaDFwVkRNZzJ1ZXg0eDYiLCJzY3AiOlsib2ZmbGluZV9hY2Nlc3MiXSwic3ViIjoiYXBpLXVzZXI0QGl3dC5uZXQifQ.EXwgGLVIpHvDObOyP5Xud_ayZW5Y3UuA7evDGVGog8EEFgr0nYw51WQLsrwbWjA9SYb-94822yn9gaJt74hVksAL9XKpfzTS1wIMgIKZjQCTh3XOX5167H5BHkHpkaATNTd4G4wjpWfTjClEKwpmGEMEeIAKQ-vTZgGCU5H5ABLNNpcrwDBLvWddLNSny1zABXdXDx1N4OYj8vglv7T27LljUjPwAZNy-fFF3s24fzf0EyPK7A9WngyYS1o_Ry_1PjaFIeIG0G3LjTtgoX8Kcan_CZrIpV6K8m619Ll-Jzon03aDJj8LNm8npcRhgemNDUUAHavUdb1pAvqquaIzhQ"
resp <- getWith opts "https://api.instantwebtools.net/v2/airlines"
return $ resp ^. responseBody

RespBody:
{
  "id": 10070,
  "name": "Srii Lankan Airways",
  "country": "Sri Lanka",
  "logo": "https://upload.wikimedia.org/wikipedia/en/thumb/9/9b/Qatar_Airways_Logo.svg/sri_lanka.png",
  "slogan": "From Sri Lanka",
  "head_quaters": "Katunayake, Sri Lanka",
  "website": "www.srilankaairways.com",
  "established": "1990"
}

```
You can try other ways of Auth using the primitivies as Above

## SSL
ssl support is enabled usings the opts.

```haskell
-- we set to the manager with openssl settings in Options and use these to build the connect using `With`.

let opts = defaults & manager .~ Left (opensslManagerSettings context)
withOpenSSL $
  getWith opts "https://httpbin.org/get"

```

## Proxy
set the proxy config in the options and use these to build the connec with `With`
``` haskell
>>> httpProxy hostName Port and returns Proxy


let opts = defaults & proxy ?~ httpProxy "localhost" 8000
getWith opts "http://httpbin.org/get"

```
# wreq-tutorial
