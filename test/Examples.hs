{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Monoid ((<>))
import           Data.JSONAPI
import           Data.Text (Text)

import           Types

-- document examples

documentText :: Text
documentText = "{\"data\":{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userName\":\"Julio\",\"userId\":2},\"links\":{\"self\":\"/api/users/2\"}}}"

documentExample :: Document User
documentExample =
  Document
    [toResource userExample]
    linksEmpty
    metaEmpty
    includedEmpty

documentMultiResourceText :: Text
documentMultiResourceText = 
  "{\"data\":[{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userName\":\"Julio\",\"userId\":2},\"links\":{\"self\":\"/api/users/2\"}}"
         <> ",{\"id\":\"3\",\"type\":\"users\",\"attributes\":{\"userName\":\"Jordi\",\"userId\":3},\"links\":{\"self\":\"/api/users/3\"}}]}"

documentMultiResourceExample :: Document User
documentMultiResourceExample =
  Document
    [toResource userExample, toResource user2Example]
    linksEmpty
    metaEmpty
    includedEmpty

userExample :: User
userExample = User 2 "Julio"

user2Example :: User
user2Example = User 3 "Jordi"

friendExample :: User
friendExample = User 4 "Johnson"

bossExample :: User
bossExample = User 5 "Smurphy"
    
documentGroupResourceText :: Text
documentGroupResourceText = 
  "{\"data\":{\"attributes\":{\"groupId\":1,\"groupName\":\"test-group\"},\"relationships\":{\"members\":{\"data\":[{\"id\":\"2\",\"type\":\"users\"},{\"id\":\"3\",\"type\":\"users\"}]}},\"id\":\"1\",\"type\":\"groups\",\"links\":{\"self\":\"/api/groups/1\"}},\"included\":[{\"attributes\":{\"userName\":\"Julio\",\"userId\":2},\"id\":\"2\",\"type\":\"users\",\"links\":{\"self\":\"/api/users/2\"}},{\"attributes\":{\"userName\":\"Jordi\",\"userId\":3},\"id\":\"3\",\"type\":\"users\",\"links\":{\"self\":\"/api/users/3\"}}]}"

documentGroupResourceExample :: Document GroupResource
documentGroupResourceExample = toDocument [groupResourceExample]

documentUserResourceText :: Text
documentUserResourceText = 
  "{\"data\":{\"attributes\":{\"userId\":2,\"userName\":\"Julio\"},\"relationships\":{\"friends\":{\"data\":{\"id\":\"4\",\"type\":\"users\"}},\"boss\":{\"data\":{\"id\":\"5\",\"type\":\"users\"}}},\"id\":\"2\",\"type\":\"users\",\"links\":{\"self\":\"/api/users/2\"}}}"

documentUserResourceExample :: Document UserResource
documentUserResourceExample = 
  Document
    [toResource userResourceExample]
    linksEmpty
    metaEmpty
    includedEmpty

userResourceExample :: UserResource
userResourceExample = UserResource userExample [friendExample] (Just bossExample) []

groupExample :: Group
groupExample = Group 1 "test-group"

groupResourceExample :: GroupResource
groupResourceExample = GroupResource groupExample [userExample,user2Example]

groupResourceResourceText :: Text
groupResourceResourceText = 
     "{\"attributes\":{\"groupId\":1,\"groupName\":\"test-group\"}"
  <> ",\"relationships\":{\"members\":{\"data\":{\"id\":\"2\",\"type\":\"users\"}"
  <> ",\"links\":{\"self\":\"/api/users/2\"}}},\"id\":\"1\",\"type\":\"groups\"}"

groupResourceResourceExample :: Resource GroupResource
groupResourceResourceExample = 
  Resource
    (Identifier "1" "groups" metaEmpty)
    (GroupResource (Group 1 "test-group") [userExample])
    linksEmpty
    (Relationships $ HM.fromList [("members", Relationship (HS.fromList [Identifier "2" "users" metaEmpty]) (Links $ HM.fromList [("self", (LinkHref "/api/users/2"))]))])

recodedGroupResourceResourceExample :: Resource GroupResource
recodedGroupResourceResourceExample = 
  Resource
    (Identifier "1" "groups" metaEmpty)
    (GroupResource (Group 1 "test-group") [])
    linksEmpty
    (Relationships $ HM.fromList [("members", Relationship (HS.fromList [Identifier "2" "users" metaEmpty]) (Links $ HM.fromList [("self", (LinkHref "/api/users/2"))]))])
  
metaText :: Text
metaText = "{\"pagination\":{\"currentPage\":1,\"totalPages\":15}}"

metaPaginationExample :: Meta
metaPaginationExample = mkMeta $ Pagination 1 15

identifierText :: Text
identifierText = "{\"id\":\"1\",\"type\":\"users\"}"

identifierExample :: Identifier
identifierExample = Identifier "1" "users" metaEmpty

identifierWithMetaText :: Text
identifierWithMetaText = "{\"id\":\"1\",\"meta\":{\"pagination\":{\"currentPage\":1,\"totalPages\":15}},\"type\":\"users\"}"

identifierWithMetaExample :: Identifier
identifierWithMetaExample = Identifier "1" "users" metaPaginationExample

linksText :: Text
linksText = "{\"self\":\"/api/users/1\",\"next\":\"/api/users/2\"}"
  
linksExample :: Links
linksExample = Links $ HM.fromList [("self",(LinkHref "/api/users/1")),("next",(LinkHref "/api/users/2"))]
  
relationshipText :: Text
relationshipText = "{\"data\":{\"id\":\"1\",\"type\":\"users\"},\"links\":{\"next\":\"/api/users/2\",\"self\":\"/api/users/1\"}}"

relationshipExample :: Relationship
relationshipExample = Relationship (HS.fromList [identifierExample]) linksExample

resourceText :: Text
resourceText = 
     "{\"id\":\"2\",\"type\":\"users\""
  <> ",\"attributes\":{\"userId\":2,\"userName\":\"Julio\"}"
  <> ",\"relationships\":{\"friend\":{\"data\":{\"id\":\"3\",\"type\":\"users\"},\"links\":{\"self\":\"/api/users/3\"}}}"
  <> "}"

resourceWithLinksText :: Text
resourceWithLinksText = "{\"id\":\"2\",\"type\":\"users\",\"attributes\":{\"userId\":2,\"userName\":\"Julio\"},\"links\":{\"self\":\"/api/users/2\",\"friend\":\"/api/users/3\"}}"

resourceExample :: Resource User
resourceExample =
  Resource
    (Identifier "2" "users" metaEmpty)
    (User 2 "Julio")
    linksEmpty
    (Relationships $ HM.fromList [("friend", Relationship (HS.fromList [Identifier "3" "users" metaEmpty]) (Links $ HM.fromList [("self", (LinkHref "/api/users/3"))]))])

resourceWithLinksExample :: Resource User
resourceWithLinksExample =
  Resource
    (Identifier "2" "users" metaEmpty)
    userExample
    (mkLinks [("self",(LinkHref "/api/users/2")),("friend",(LinkHref "/api/users/3"))])
    relationshipsEmpty

