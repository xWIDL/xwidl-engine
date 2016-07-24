-- Translate WebIDL into xWIDL
module WebIDL where

import qualified Language.WebIDL.AST as W
import Spec
import Language.WebIDL.Parser
import JS.Type

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.String.Utils

data TransState = TransState {
    _emitted :: M.Map Name Definition,
    _focus   :: Definition
}

-- Interface translation
type Trans = ExceptT String (State TransState)

transIface :: W.Interface Tag -> Trans ()
transIface (W.Interface _ extAttrs iname mInherit members) = do
    replaceFocus (DefInterface (Interface (i2n iname) [] M.empty M.empty M.empty))
    case mInherit of
        Just pid -> transIfaceInherit pid
        Nothing  -> return ()
    mapM_ transExtAttr extAttrs
    mapM_ transIfaceMember members

transExtAttr :: W.ExtendedAttribute Tag -> Trans ()
transExtAttr = \case
    W.ExtendedAttributeArgList tag (W.Ident "Constructor") args -> do
        let (mEns, mReq) = analyzeConsAnn $ _comment tag
        args' <- mapM transArg args
        emitConstructor (InterfaceConstructor args' mEns mReq)
    _ -> return () -- TODO

transIfaceMember :: W.InterfaceMember Tag -> Trans ()
transIfaceMember = \case
    W.IMemConst _ -> return () -- TODO
    W.IMemAttribute attr -> transIfaceAttr attr
    W.IMemOperation op -> transIfaceOp op

transIfaceAttr :: W.Attribute Tag -> Trans ()
transIfaceAttr (W.Attribute tag _mInheritModifier _mReadOnlyModifer ty x) = do
    inspectAttrComment $ _comment tag
    ty' <- transType ty
    emitAttr ty' (i2n x)

-- If an operation has no identifier, then it must be declared to be a special operation using one of the special keywords.
transIfaceOp :: W.Operation Tag -> Trans ()
transIfaceOp (W.Operation tag _extAttrs _mQualifier ret (Just f) args) = do
    (mEns, mReq) <- analyzeOpAnn $ _comment tag
    args' <- mapM transArg args
    ret' <- transRet ret
    emitOp $ Operation (i2n f) args' ret' mEns mReq

transIfaceInherit :: W.Ident -> Trans ()
transIfaceInherit _ = return () -- TODO

transRet :: W.ReturnType -> Trans (Maybe Type)
transRet = \case
    W.RetType ty -> Just <$> transType ty
    W.RetVoid    -> return Nothing

transType :: W.Type -> Trans Type
transType = \case
    W.TySingleType singleTy -> transSingleType singleTy
    W.TyUnionType unionTy tySuffix -> do
        ty <- transUnionType unionTy
        return (applyTySuffix tySuffix ty)

transSingleType :: W.SingleType -> Trans Type
transSingleType = \case
    W.STyNonAny nonAny -> transNonAnyType nonAny
    W.STyAny suffix -> return (applyTySuffix suffix TyAny)

transNonAnyType :: W.NonAnyType -> Trans Type
transNonAnyType = \case
    W.TyPrim primTy suffix -> applyTySuffix suffix <$> transPrimType primTy
    W.TyDOMString suffix -> return (applyTySuffix suffix TyDOMString)
    W.TyIdent i suffix -> return (applyTySuffix suffix (TyInterface (i2n i)))
    W.TySequence ty mNull -> do
        ty' <- transType ty
        case mNull of
            Just W.Null -> return (TyNullable ty')
            Nothing   -> return ty'
    W.TyObject suffix -> return (applyTySuffix suffix TyObject)
    W.TyDate suffix -> return (applyTySuffix suffix $ TyBuiltIn (Name "Date"))

applyTySuffix ::  W.TypeSuffix -> Type -> Type
applyTySuffix W.TypeSuffixArray ty = TyArray ty
applyTySuffix W.TypeSuffixNullable ty = TyNullable ty
applyTySuffix W.TypeSuffixNone ty = ty

transUnionType :: W.UnionType -> Trans Type
transUnionType tys = TyUnion <$> mapM transUMType tys

transUMType :: W.UnionMemberType -> Trans Type
transUMType = \case
    W.UnionTy ut suffix -> applyTySuffix suffix <$> transUnionType ut
    W.UnionTyNonAny nonAny -> transNonAnyType nonAny
    W.UnionTyAny suffix -> return (applyTySuffix suffix TyAny)

transPrimType :: W.PrimitiveType -> Trans Type
transPrimType = \case
    W.PrimIntegerType _ -> return TyInt
    W.PrimFloatType _   -> return TyFloat
    W.Boolean           -> return TyBoolean
    W.Byte              -> return (TyBuiltIn $ Name "Byte")
    W.Octet             -> return (TyBuiltIn $ Name "Octet")

transArg :: W.Argument Tag -> Trans Argument
transArg = \case
    W.ArgOptional _extAttrs ty (W.ArgIdent x) mDefault -> do
        ty' <- transType ty
        return $ Argument (i2n x) ty' mDefault
    W.ArgNonOpt _extAttrs ty _mEllipsis (W.ArgIdent x) -> do
        ty' <- transType ty
        return $ Argument (i2n x) ty' Nothing

inspectAttrComment :: [String] -> Trans ()
inspectAttrComment = mapM_ collectGhostAttr .
                     filter isGhostAttrComment
    where
        collectGhostAttr s = do
            let attrStr = drop 5 (strip s)
            case tryParse pAttribute attrStr of
                Right (W.Attribute _ _mInheritModifier _mReadOnlyModifer ty x) -> do
                    ty' <- transType ty
                    emitGhostAttr ty' (i2n x)
                Left err -> throwError $ "Parse ghost attribute error: " ++ show err
        isGhostAttrComment  = startswith "ghost" . strip

analyzeConsAnn :: [String] -> (Maybe String, Maybe String)
analyzeConsAnn = analyzeFunAnn

analyzeFunAnn :: [String] -> (Maybe String, Maybe String)
analyzeFunAnn blocks =
    let ens      = fmap strip $ listToMaybe $ filter isEnsureComment blocks
        req      = fmap strip $ listToMaybe $ filter isRequireComment blocks
    in  (ens, req)
    where
        isEnsureComment  = startswith "ensures" . strip
        isRequireComment = startswith "requires" . strip

analyzeOpAnn :: [String] -> Trans (Maybe String, Maybe String)
analyzeOpAnn blocks = do
    inspectAttrComment blocks
    return $ analyzeFunAnn blocks

emitAttr :: Type -> Name -> Trans ()
emitAttr ty x =
    modify (\s -> let DefInterface iface = (_focus s)
                      focus' = iface { _attrs = M.insert x ty (_attrs iface) }
                  in  s { _focus = DefInterface focus' })

emitGhostAttr :: Type -> Name -> Trans ()
emitGhostAttr ty x =
    modify (\s -> let DefInterface iface = (_focus s)
                      focus' = iface { _ghostAttrs = M.insert x ty (_ghostAttrs iface) }
                  in  s { _focus = DefInterface focus' })

emitConstructor :: InterfaceConstructor -> Trans ()
emitConstructor cons =
    modify (\s -> let DefInterface iface = (_focus s)
                      focus' = iface { _constructors = cons : _constructors iface }
                  in  s { _focus = DefInterface focus' })

emitOp :: Operation -> ExceptT String (State TransState) ()
emitOp op =
    modify (\s -> let DefInterface iface = (_focus s)
                      focus' = iface { _operations = M.insert (_imName op) op (_operations iface) }
                  in  s { _focus = DefInterface focus' })

i2n :: W.Ident -> Name
i2n (W.Ident x) = Name x

replaceFocus :: Definition -> Trans ()
replaceFocus def = do
    modify (\s -> s { _emitted = M.insert (nameOf (_focus s)) (_focus s) (_emitted s), _focus = def })

nameOf :: Definition -> Name
nameOf (DefInterface i) = _iName i
nameOf (DefDictionary (Dictionary name _)) = name
nameOf (DefException (Exception name _)) = name
nameOf (DefEnum (Enum name _)) = name
