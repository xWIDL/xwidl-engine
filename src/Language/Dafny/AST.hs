-- Dafny AST and printer
module Language.Dafny.AST where

import Text.PrettyPrint.Leijen
import Language.JS.Type
import Language.JS.Platform (RelBiOp(..))

import qualified Data.Map as M

data Trait = Trait {
    _tname :: String,
    _tattrs :: M.Map String TraitMemberAttr,
    _tmethods :: M.Map String TraitMemberMethod
} deriving (Show)

type TraitMemberAttr = (String, DyType)

data TraitMemberMethod = TraitMemberMethod {
    _tmName :: String,
    _tmArgs :: [(String, DyType)],
    _tmRet  :: Maybe (String, DyType),
    -- from xWIDL
    _tmEnsures :: Maybe String,
    -- from xWIDL
    _tmRequires :: Maybe String
    -- from xWIDL
    -- _tmImpl :: Maybe String
} deriving (Show)

data TopLevelMethod = TopLevelMethod {
    _tlName :: String,
    _tlArgs :: M.Map String DyType,
    -- generated conjunctive form
    _tlRequires :: [DyExpr],
    _tlBody :: [Stmt]
} deriving (Show)

data DyVal = DVar String
           | DPrim Prim
           -- | DSeq [DyVal]
           deriving (Show)

data DyExpr = DVal DyVal
            | DCall String String [DyExpr]
            | DAccess String String
            | DRel RelBiOp DyExpr DyExpr
            deriving (Show)

data Stmt = SVarDecl String DyType -- var x : <type>;
          | SVarDef String DyExpr -- var x := <expr>;
          | SVarAssign String DyExpr -- x := <expr>;
          | SInvoke String String [DyExpr] -- x.f([<expr>]);
          | SAssert DyExpr -- assert <expr>;
          deriving (Show)

data DyType = DTyClass String
            | DTyString
            | DTyInt
            | DTyBool
            | DTyReal
            deriving (Show)

-- Pretty print

instance Pretty Trait where
    pretty (Trait t ma mm) = text "trait" <+> text t <+>
                             braces (line <> vsep (map (indent 4 . prettyAttr) (M.elems ma) ++
                                                   map (indent 4 . pretty) (M.elems mm)) <> line)

instance Pretty TopLevelMethod where
    pretty (TopLevelMethod x args requires body) =
        text "method" <+> text x <>
        parens (hcat (punctuate (comma <> space) (map prettySig (M.toList args)))) <> line <>
        (if length requires > 0 then
            indent 4 (text "requires" <+> (hcat (punctuate (space <> text "&&" <> space) (map pretty requires))))
            else empty) <+>
        braces (line <> vsep (map (\s -> indent 4 (pretty s) <> semi) body) <> line)

instance Pretty TraitMemberMethod where
    pretty (TraitMemberMethod x args mRet ensures requires) =
        text "method" <+> text x <>
        parens (hcat (punctuate (comma <> space) (map prettySig args))) <+>
        prettyMaybe mRet (\ret -> text "returns" <+> parens (prettySig ret)) <> line <>
        prettyMaybe requires (\req -> indent 4 (text "requires" <+> text req <> line)) <>
        prettyMaybe ensures (\ens -> (text "ensures" <+> text ens <> line))-- <>
        -- prettyMaybe impl (\imp -> line <> text imp <> line)

prettySig :: (String, DyType) -> Doc
prettySig (x, ty) = text x <> text ":" <+> pretty ty

prettyAttr :: (String, DyType) -> Doc
prettyAttr pair = text "var" <+> prettySig pair <> line

instance Pretty DyType where
    pretty (DTyClass x) = text x
    pretty DTyString = text "string"
    pretty DTyInt = text "int"
    pretty DTyBool = text "bool"
    pretty DTyReal = text "real"

instance Pretty DyExpr where
    pretty (DVal v) = pretty v
    pretty (DCall x f args) = text x <> text "." <> text f <>
                              parens (hcat (punctuate (comma <> space) (map pretty args)))
    pretty (DAccess x attr) = text (x ++ "." ++ attr)
    pretty (DRel op a b) = pretty a <+> pretty op <+> pretty b

instance Pretty DyVal where
    pretty (DVar x) = text x
    pretty (DPrim p) = pretty p
    -- pretty (DSeq es) = text "[" <> (hcat (punctuate (comma <> space) (map pretty es))) <> text "]"


instance Pretty RelBiOp where
    pretty = \case
        NotEqual -> text "!="
        Equal -> text "=="
        And -> text "&&"
        LessThan -> text "<"
        LessEq -> text "<="
        GreaterThan -> text ">"
        GreaterEq -> text ">="

instance Pretty Stmt where
    pretty (SVarDef x e) = text "var" <+> text x <+> text ":=" <+> pretty e
    pretty (SVarDecl x cls) = text "var" <+> text x <+> text ":" <+> pretty cls
    pretty (SVarAssign x e) = text x <+> text ":=" <+> pretty e
    pretty (SInvoke x f args) = text x <> text "." <> text f <>
                                parens (hcat (punctuate (comma <> space) (map pretty args)))
    pretty (SAssert e) = text "assert" <+> pretty e

prettyMaybe :: Maybe t -> (t -> Doc) -> Doc
prettyMaybe Nothing  _ = empty
prettyMaybe (Just x) f = f x
