# json-api

Forked from [toddmohney/json-api](https://github.com/toddmohney/json-api)

## Representing JSON API in Haskell

A Haskell API using JSON API should provide only Documents. Document is the top 
level type and all of the other types are included within a Document.

- The list of `Identifier`s in `Relationship` must be unique, there is an `Eq` 
constraint to make sure it can remove duplications.
- The self link (if it exists) in a Relationship should return all of the 
`Identifier`s in that `Relationship`.
- A `Relationship` must have a list of `Identifier`s and/or `Links` but both 
cannot be empty, that is why there is a `mkRelationship` function.
- `Meta`, `Links`, `Relationships` and `Included` may all be empty. If they are 
empty the key will not exist in the JSON representation.
- A `Document` has at least one `Resource`.
- Use `toDocument` and `fromDocument` when making and receiving `Document`. You can 
also use `toDocumentComplete` and `fromDocumentComplete` if you need `Links` and `Meta`.
- If a `Document` has `Relationships` its `toJSON` and `fromJSON` will generally 
not commute because `toJSON` and `fromJSON` do not know how to encode `Relationships` data from a `Resource`
into and from `docRelationships`. If you need to commute, derive and use `DocumentEntity`.