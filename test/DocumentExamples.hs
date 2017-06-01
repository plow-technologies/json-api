{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DocumentExamples where
  
import Data.JSONAPI
import Types
import Test.Hspec

main :: IO ()
main = 
  hspec $ do
    documentSpec "Document with a single Resource" docWithSingleResource
    documentSpec "Document with Links" docWithLinks
    documentSpec "Document with Meta" docWithMeta
    documentSpec "Document with Links and Meta" docWithLinksAndMeta
    documentSpec "Document with a single Relationships" docWithRelationships
    documentSpec "Document with a similarly typed Relationships but different keys" docWithSimilarlyTypedRelationships
    documentSpec "Document with heteorogeneous Relationships" docWithHeterogeneousRelationships
    documentSpec "Document with multiple Resources" docWithMultipleResources
        
documentSpec :: forall a. (DocumentEntity a, Show a, Eq a) => String -> Document a -> Spec
documentSpec s doc = 
  describe s $
    it "fromDocumentComplete then toDocumentComplete should produce the original document" $ ((uncurry3 toDocumentComplete) . fromDocumentComplete $ doc) `shouldBe` doc

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f = \(x,y,z) -> f x y z

{-
document 
document with links
document with meta (pagination)
document with links and meta
document with relationships
document with similar typed relationships but different keys
document with multiple resources
document with heterogeneous relationships
-}

user1 :: User
user1 = User 1 "Julio"

userResource1 :: UserResource
userResource1 = UserResource user1 [] Nothing []

docWithSingleResource :: Document UserResource
docWithSingleResource = toDocument [userResource1]

docWithLinks :: Document UserResource
docWithLinks = doc { docLinks = mkSimpleLinks [("self","/user/1"),("next","/user/2")] }
  where
    doc = toDocument [userResource1]

docWithMeta :: Document UserResource
docWithMeta = doc { docMeta = mkMeta $ Pagination 1 1 }
  where
    doc = toDocument [userResource1]

docWithLinksAndMeta :: Document UserResource
docWithLinksAndMeta = doc { docLinks = mkSimpleLinks [("self","/user/1"),("next","/user/2")]
                          , docMeta = mkMeta $ Pagination 1 1 }
  where
    doc = toDocument [userResource1]

user2 :: User
user2 = User 2 "Jordi"

user3 :: User
user3 = User 3 "Shulhi"

user4 :: User
user4 = User 4 "Smurphy"

userResource2 :: UserResource
userResource2 = UserResource user1 [user2,user3] Nothing []

docWithRelationships :: Document UserResource
docWithRelationships = toDocument [userResource2]

userResource3 :: UserResource
userResource3 = UserResource user1 [user2,user3] (Just user4) []

docWithSimilarlyTypedRelationships :: Document UserResource
docWithSimilarlyTypedRelationships = toDocument [userResource3]

blogPost1 :: BlogPost
blogPost1 = BlogPost 1 "Hello"

blogPost2 :: BlogPost
blogPost2 = BlogPost 2 "Goodbye"

userResource4 :: UserResource
userResource4 = UserResource user1 [user2,user3] (Just user4) [blogPost1, blogPost2]

docWithHeterogeneousRelationships :: Document UserResource
docWithHeterogeneousRelationships = toDocument [userResource4]

user5 :: User
user5 = User 5 "Daniel"

user6 :: User
user6 = User 6 "Brent"

user7 :: User
user7 = User 7 "Jeremy"

blogPost3 :: BlogPost
blogPost3 = BlogPost 3 "Guten Tag"

userResource5 :: UserResource
--userResource5 = UserResource user5 [user2,user6] (Just user7) [blogPost3]
userResource5 = UserResource user5 [user6] (Just user7) [blogPost3]
docWithMultipleResources :: Document UserResource
docWithMultipleResources = toDocument [userResource4, userResource5]
