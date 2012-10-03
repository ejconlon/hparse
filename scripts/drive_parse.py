#!/usr/bin/env python

import json
import urllib
import urllib2

parts = [
    "(terminal lcurly)"
  , "(terminal colon)"
  , "(terminal comma)"
  , "(terminal rcurly)"
  , "(terminal dquote)"
  , "(combinator not (x))"
  , "(combinator star (x))"
  , "(combinator maybe (x))"
  , "(combinator or (x y))"
  , "(production string (dquote (star (not dquote)) dquote))"
  , "(production pair (string colon string))"
  , "(production pairlist (or (pair) (pair comma pairlist)))"
  , "(production object (lcurly (maybe pairlist) rcurly))"
  ]

program = "(program\n  "+"\n  ".join(parts)+")"
print program

url = 'http://localhost:8000/parse'
values = {'program' : program}

data = urllib.urlencode(values)
req = urllib2.Request(url, data)
response = urllib2.urlopen(req)
body = response.read()
model = json.loads(body)

print model

