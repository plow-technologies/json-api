{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

#if defined(ghcjs_HOST_OS)
main :: IO ()
main = putStrLn "persistent-test does not run in GHCJS"  
#else
import Database.Persist.Sqlite
import Data.JSONAPI
import Models
import Test.Hspec

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f = \(x,y,z) -> f x y z

documentSpec :: forall a. (DocumentEntity a, Show a, Eq a) => String -> Document a -> Spec
documentSpec s doc = 
  describe s $
    it "fromDocumentComplete then toDocumentComplete should produce the original document" $ ((uncurry3 toDocumentComplete) . fromDocumentComplete $ doc) `shouldBe` doc

main :: IO ()
main = do 
  userDoc <- runSqlite ":memory:" $ do
    runMigration migrateAll
    
    let user1  = User "Smurphy"
        user2  = User "Brent"
        group1 = Group "Plowtech"
    user1Id  <- insert user1
    user2Id  <- insert user2
    group1Id <- insert group1
    
    let groupUserJoin1 = GroupUserJoin user1Id group1Id
    
    _groupUserJoin1Id <- insert groupUserJoin1
    
    let blogPost1 = BlogPost "Hello" user1Id
        blogPost2 = BlogPost "Goodbye" user2Id 
    
    _blogPost1Id <- insert blogPost1
    _blogPost2Id <- insert blogPost2
    
    user  <- head <$> selectList [UserName ==. "Smurphy"] []
    guj   <- head <$> selectList [GroupUserJoinUserId ==. (entityKey user)] []
    group <- head <$> selectList [GroupId ==. (groupUserJoinGroupId . entityVal $ guj)] []
    bps   <-          selectList [BlogPostUserId ==. (entityKey user)] []
    return $ toDocument [UserResource user [group] bps]
    
  hspec $ do
    documentSpec "UserResource" userDoc
#endif