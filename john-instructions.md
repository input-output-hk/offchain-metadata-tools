Hey John!

I didn't manage to test the NixOS service and release.nix, but I did
go through and change what I thought needed to be changed.

## Testing

If you manage to get something spun up, you can test it is working with the following:

- Execute the psql statements defined in [john.sql](./john.sql):

```
  psql -d <database-name> -f john.sql
```

  (The write API isn't finished so you just have to manually insert
  some data into the database).

- Then perform the CURL commands listed in
  [john-curl.txt](./john-curl.txt) and check the response against the
  expected response.
  
## API

The public API is pretty simple, and you can see examples of each
method in the [john-curl.txt](./john-curl.txt) file.

The world needs to be able to reach the following endpoints:

### GET metadata/{subject}
Return the value for all properties associated with the subject.
example

### GET metadata/{subject}/properties
Same as above.

### GET metadata/{subject}/properties/{property}
Return the value for the given property associated with the subject.
example

### POST metadata/query
This endpoint provides a way to batch queries, making several requests
of the server with only one HTTP request.

The request body should include a JSON object with the following keys:

    “subjects” : A list of subjects, encoded as strings.
    “properties” : An optional list of property names, encoded as strings.

If only “subjects” is supplied, this query will return a list of
subjects with all their properties
