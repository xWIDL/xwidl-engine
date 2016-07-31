-- Translate WebIDL into xWIDL
module WebIDL (transDefsToSpec) where

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
    _focus   :: Definition,
    _typemap :: M.Map Name Type
}

-- Interface translation
type Trans = StateT TransState (Except String)


transDefsToSpec :: [W.Definition Tag] -> Either String Spec
transDefsToSpec defs = do
    s <- runExcept (flip execStateT initState $ do
                mapM_ transDef defs
                replaceFocus dummyDef
                )
    let defsMap = M.delete dummyName (_emitted s)
    -- ((a0 -> b0 -> b0) -> b0 -> t0 a0 -> b0))
    return (foldr distribute (Spec M.empty M.empty M.empty M.empty) (M.toList defsMap))
    where
        initState = TransState {
            _emitted = M.empty,
            _focus   = dummyDef,
            _typemap = M.empty
        }
        dummyName = Name ""
        dummyDef = DefInterface (Interface dummyName [] M.empty M.empty M.empty)
        distribute (x, DefInterface i) s = s { _ifaces = M.insert x i (_ifaces s) }
        distribute (x, DefDictionary d) s = s { _dicts = M.insert x d (_dicts s) }
        distribute (x, DefException e) s = s { _exceptions = M.insert x e (_exceptions s) }
        distribute (x, DefEnum e) s = s { _enums = M.insert x e (_enums s) }


transDef :: W.Definition Tag -> Trans ()
transDef = \case
    W.DefInterface i -> transIface i
    W.DefPartial p -> case p of
        W.PartialInterface _ x members -> transPartialIface x members
        W.PartialDictionary _ x members -> transPartialDict x members
    W.DefDictionary dict -> transDict dict
    W.DefException e -> transException e
    W.DefEnum e -> transEnum e
    W.DefTypedef def -> transTypeDef def
    W.DefImplementsStatement _ -> throwError "`implements` is not supported yet"

transIface :: W.Interface Tag -> Trans ()
transIface (W.Interface _ extAttrs iname mInherit members) =
    transIface' iname $ do
        justDoIt mInherit transIfaceInherit
        mapM_ transExtAttr extAttrs
        mapM_ transIfaceMember members

transPartialIface :: W.Ident -> [W.InterfaceMember Tag] -> Trans ()
transPartialIface iname members = transIface' iname $ mapM_ transIfaceMember members

transIface' :: W.Ident -> Trans () -> Trans ()
transIface' i work = do
    m <- _emitted <$> get
    case M.lookup (i2n i) m of
        Just (DefInterface iface) -> do
            replaceFocus (DefInterface iface)
            work
        Just _ -> throwError $ "Invalid interface name: " ++ show i
        Nothing -> do
            replaceFocus (DefInterface (Interface (i2n i) [] M.empty M.empty M.empty))
            work

transDict :: W.Dictionary Tag -> Trans ()
transDict (W.Dictionary _ dname mInherit dmembers) =
    mapM transDictMember dmembers >>= transDict' dname (justDoIt mInherit transDictInherit)

transPartialDict :: W.Ident -> [W.DictionaryMember Tag] -> Trans ()
transPartialDict dname members = mapM transDictMember members >>= transDict' dname (return ())

transDict' :: W.Ident -> Trans () -> [DictionaryMember] -> Trans ()
transDict' i work members = do
    m <- _emitted <$> get
    case M.lookup (i2n i) m of
        Just (DefDictionary dict) ->
            replaceFocus (DefDictionary (dict { _dmembers = (_dmembers dict) ++ members })) >> work
        Just _ -> throwError $ "Invalid dictionary name: " ++ show i
        Nothing ->
            replaceFocus (DefDictionary (Dictionary (i2n i) members)) >> work

transException :: W.Exception Tag -> Trans ()
transException (W.Exception _ x mInherit members) = do
    justDoIt mInherit transExceptionInherit
    replaceFocus (DefException (Exception (i2n x) []))
    mapM_ transExceptionMember members

transEnum :: W.Enum Tag -> Trans ()
transEnum (W.Enum _ i evals) = replaceFocus (DefEnum (Enum (i2n i) (map (\(W.EnumValue s) -> s) evals)))

transTypeDef :: W.Typedef Tag -> Trans ()
transTypeDef (W.Typedef _ ty i) = do
    ty' <- transType ty
    modify (\s -> s { _typemap = M.insert (i2n i) ty' (_typemap s) })

transExtAttr :: W.ExtendedAttribute Tag -> Trans ()
transExtAttr = \case
    W.ExtendedAttributeArgList tag (W.Ident "Constructor") args -> do
        let (mEns, mReq) = analyzeConsAnn $ _comment tag
        args' <- mapM transArg args
        emitConstructor (InterfaceConstructor args' mEns mReq)
    W.ExtendedAttributeNoArgs tag (W.Ident "Constructor") -> do
        let (mEns, mReq) = analyzeConsAnn $ _comment tag
        emitConstructor (InterfaceConstructor [] mEns mReq)
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

transDictInherit :: W.Ident -> Trans ()
transDictInherit _ = return () -- TODO

transExceptionInherit :: W.Ident -> Trans ()
transExceptionInherit _ = return () -- TODO

transDictMember :: W.DictionaryMember Tag -> Trans DictionaryMember
transDictMember (W.DictionaryMember _ ty i mDef) = do
    ty' <- transType ty
    return (DictionaryMember ty' (i2n i) mDef)

transExceptionMember :: W.ExceptionMember Tag -> Trans ExceptionMember
transExceptionMember = \case
    W.ExConst _ _ -> error "Const is not supported yet" -- TODO
    W.ExField _ ty i -> do
        ty' <- transType ty
        return (ExField ty' (i2n i))

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
    W.TyIdent i suffix -> do
        let name = i2n i
        tymap <- _typemap <$> get
        case M.lookup name tymap of
            Just ty -> return ty
            Nothing -> return $ applyTySuffix suffix (TyInterface name)
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
            let attrStr = strip $ drop 8 (strip s)
            case tryParse pAttribute attrStr of
                Right (W.Attribute _ _mInheritModifier _mReadOnlyModifer ty x) -> do
                    ty' <- transType ty
                    emitGhostAttr ty' (i2n x)
                Left err -> throwError $ "Parse ghost attribute error: " ++ show err
        isGhostAttrComment  = startswith "/- ghost" . strip

analyzeConsAnn :: [String] -> (Maybe String, Maybe String)
analyzeConsAnn = analyzeFunAnn

analyzeFunAnn :: [String] -> (Maybe String, Maybe String)
analyzeFunAnn blocks =
    let ens      = fmap (strip . drop 10 . strip) $ listToMaybe $ filter isEnsureComment blocks
        req      = fmap (strip . drop 11 . strip) $ listToMaybe $ filter isRequireComment blocks
    in  (ens, req)
    where
        isEnsureComment  = startswith "/- ensures" . strip
        isRequireComment = startswith "/- requires" . strip

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

emitOp :: Operation -> Trans ()
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

justDoIt (Just a) f = f a
justDoIt Nothing _ = return ()
