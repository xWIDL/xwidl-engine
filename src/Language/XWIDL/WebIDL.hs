-- Translate WebIDL into xWIDL
module Language.XWIDL.WebIDL (
  transDefsToSpec, analyzeCBS
) where

import qualified Language.WebIDL.AST as W
import Language.XWIDL.Spec
import Language.WebIDL.Parser
import Language.JS.Type

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Either (partitionEithers)
import Data.String.Utils

data TransState = TransState {
    _emitted :: M.Map Name Definition,
    _focus   :: Definition,
    _typemap :: M.Map Name IType_
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
    return (foldr distribute (Spec M.empty M.empty M.empty M.empty) (M.toList defsMap))
    where
        initState = TransState {
            _emitted = M.empty,
            _focus   = dummyDef,
            _typemap = M.empty
        }
        dummyName = Name ""
        dummyDef = DefInterface (Interface dummyName Nothing (InterfaceConstructors []) M.empty M.empty M.empty M.empty)
        distribute (x, DefInterface i) s = s { _ifaces = M.insert x i (_ifaces s) }
        distribute (x, DefDictionary d) s = s { _dicts = M.insert x d (_dicts s) }
        distribute (x, DefException e) s = s { _exceptions = M.insert x e (_exceptions s) }
        distribute (x, DefCallback c) s = s { _cbs = M.insert x c (_cbs s) }

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
    W.DefCallback c -> transCallback c

transCallback :: W.Callback Tag -> Trans ()
transCallback (W.Callback _ i retty args) = do
    retty' <- transRet retty
    (args', _) <- partitionArgs <$> mapM transArg args -- XXX: opt args
    replaceFocus $ DefCallback (Callback (i2n i) retty' args')

transIface :: W.Interface Tag -> Trans ()
transIface (W.Interface _ extAttrs iname mInherit members) =
    transIface' iname (Just $ fmap i2n mInherit) $ do
        mapM_ transExtAttr extAttrs
        mapM_ transIfaceMember members

transPartialIface :: W.Ident -> [W.InterfaceMember Tag] -> Trans ()
transPartialIface iname members = transIface' iname Nothing $ mapM_ transIfaceMember members

transIface' :: W.Ident -> Maybe (Maybe Name) -> Trans () -> Trans ()
transIface' i mChangeInherit work = do
    m <- _emitted <$> get
    case M.lookup (i2n i) m of
        Just (DefInterface iface) -> do
            case mChangeInherit of
                Just mInherit -> replaceFocus (DefInterface iface { _iInherit = mInherit })
                Nothing       -> replaceFocus (DefInterface iface)
            work
        Just _ -> throwError $ "Invalid interface name: " ++ show i
        Nothing -> do
            replaceFocus (DefInterface (Interface (i2n i)
                                                  (fromMaybe Nothing mChangeInherit)
                                                  (InterfaceConstructors [])
                                                  M.empty M.empty M.empty M.empty))
            work

transDict :: W.Dictionary Tag -> Trans ()
transDict (W.Dictionary _ dname mInherit dmembers) =
    mapM transDictMember dmembers >>= transDict' dname (Just $ fmap i2n mInherit)

transPartialDict :: W.Ident -> [W.DictionaryMember Tag] -> Trans ()
transPartialDict dname members = mapM transDictMember members >>= transDict' dname Nothing

transDict' :: W.Ident -> Maybe (Maybe Name) -> [DictionaryMember] -> Trans ()
transDict' i mmInhereit members = do
    m <- _emitted <$> get
    case M.lookup (i2n i) m of
        Just (DefDictionary dict) ->
            case mmInhereit of
                Just mInherit -> do
                    let dict' = dict { _dmembers = (_dmembers dict) ++ members , _dInherit = mInherit }
                    replaceFocus (DefDictionary dict')
                Nothing -> do
                    let dict' = dict { _dmembers = (_dmembers dict) ++ members }
                    replaceFocus (DefDictionary dict')
        Just _ -> throwError $ "Invalid dictionary name: " ++ show i
        Nothing ->
            replaceFocus (DefDictionary (Dictionary (i2n i) (fromMaybe Nothing mmInhereit) members))

transException :: W.Exception Tag -> Trans ()
transException (W.Exception _ x mInherit members) = do
    replaceFocus (DefException (Exception (i2n x) (fmap i2n mInherit) []))
    mapM_ transExceptionMember members

transEnum :: W.Enum Tag -> Trans ()
transEnum (W.Enum _ i _) = modify (\s -> s { _typemap = M.insert (i2n i) (ITySingle TyDOMString) (_typemap s) })

transTypeDef :: W.Typedef Tag -> Trans ()
transTypeDef (W.Typedef _ ty i) = do
    ty' <- transType ty
    modify (\s -> s { _typemap = M.insert (i2n i) ty' (_typemap s) })

transExtAttr :: W.ExtendedAttribute Tag -> Trans ()
transExtAttr = \case
    W.ExtendedAttributeArgList tag (W.Ident "Constructor") args -> do
        let (_, mReq, mEff) = analyzeConsAnn $ _comments tag
        (args', optArgs') <- partitionArgs <$> mapM transArg args -- XXX: opt args
        emitConstructor (InterfaceConstructor args' optArgs' mReq mEff)
    W.ExtendedAttributeNoArgs tag (W.Ident "Constructor") -> do
        let (_, mReq, mEff) = analyzeConsAnn $ _comments tag
        emitConstructor (InterfaceConstructor [] [] mReq mEff)
    W.ExtendedAttributeNoArgs tag (W.Ident "HTMLConstructor") -> emitHTMLConstructor
    _ -> return () -- TODO

transIfaceMember :: W.InterfaceMember Tag -> Trans ()
transIfaceMember = \case
    W.IMemConst (W.Const _ cty i cv) -> emitConst cty i cv
    W.IMemAttribute attr -> transIfaceAttr attr
    W.IMemOperation op -> transIfaceOp op

transIfaceAttr :: W.Attribute Tag -> Trans ()
transIfaceAttr (W.Attribute tag _mInheritModifier _mReadOnlyModifer ty x) = do
    inspectAttrComment $ _comments tag
    ITySingle ty' <- transType ty
    emitAttr ty' (i2n x)

-- If an operation has no identifier, then it must be declared to be a special operation using one of the special keywords.
transIfaceOp :: W.Operation Tag -> Trans ()
transIfaceOp (W.Operation tag _extAttrs _mQualifier ret (Just f) args) = do
    (mEns, mReq, mEff, cbs) <- analyzeOpAnn $ _comments tag
    (args', optArgs') <- partitionArgs <$> mapM transArg args
    ret' <- transRet ret
    emitOp $ Operation (i2n f) args' optArgs' ret' mEns mReq mEff cbs
transIfaceOp _ = error "Special operations are not supported yet"

transDictMember :: W.DictionaryMember Tag -> Trans DictionaryMember
transDictMember (W.DictionaryMember _ ty i mDef) = do
    ITySingle ty' <- transType ty
    return (DictionaryMember ty' (i2n i) mDef)

transExceptionMember :: W.ExceptionMember Tag -> Trans ExceptionMember
transExceptionMember = \case
    W.ExConst _ _ -> error "Const is not supported yet" -- TODO
    W.ExField _ ty i -> do
        ITySingle ty' <- transType ty
        return (ExField ty' (i2n i))

transRet :: W.ReturnType -> Trans (Maybe IType)
transRet = \case
    W.RetType ty -> transType ty >>= (\(ITySingle ty) -> return (Just ty))
    W.RetVoid    -> return Nothing

transType :: W.Type -> Trans IType_
transType = \case
    W.TySingleType singleTy -> ITySingle <$> transSingleType singleTy
    W.TyUnionType unionTy tySuffix -> do
        tys <- transUnionType unionTy
        return $ ITyUnion (map (applyTySuffix tySuffix) tys)

transSingleType :: W.SingleType -> Trans IType
transSingleType = \case
    W.STyNonAny nonAny -> transNonAnyType nonAny
    W.STyAny suffix -> return (applyTySuffix suffix TyAny)

transNonAnyType :: W.NonAnyType -> Trans IType
transNonAnyType = \case
    W.TyPrim primTy suffix -> return $ applyTySuffix suffix (transPrimType primTy)
    W.TyDOMString suffix -> return (applyTySuffix suffix TyDOMString)
    W.TyIdent i suffix -> do
        let name = i2n i
        tymap <- _typemap <$> get
        case M.lookup name tymap of
            Just (ITySingle ty) -> return ty
            Just _ -> error "tyindent union"
            Nothing -> case unName name of
                            "USVString" -> return $ TyDOMString
                            _ -> return $ applyTySuffix suffix (TyInterface name)
    W.TySequence ty mNull -> do
        ITySingle ty' <- transType ty
        case mNull of
            Just W.Null -> return (TyNullable ty')
            Nothing   -> return ty'
    W.TyObject suffix -> return (applyTySuffix suffix TyObject)
    W.TyDate suffix -> return (applyTySuffix suffix $ TyBuiltIn (Name "Date"))

applyTySuffix ::  W.TypeSuffix -> IType -> IType
applyTySuffix W.TypeSuffixArray ty = TyArray ty
applyTySuffix W.TypeSuffixNullable ty = TyNullable ty
applyTySuffix W.TypeSuffixNone ty = ty

transUnionType :: W.UnionType -> Trans [IType]
transUnionType tys = mapM transUMType tys

transUMType :: W.UnionMemberType -> Trans IType
transUMType = \case
    W.UnionTy ut suffix -> error "union in union" -- applyTySuffix suffix <$> transUnionType ut
    W.UnionTyNonAny nonAny -> transNonAnyType nonAny
    W.UnionTyAny suffix -> return (applyTySuffix suffix TyAny)

transPrimType :: W.PrimitiveType -> IType
transPrimType = \case
    W.PrimIntegerType _ -> TyInt
    W.PrimFloatType _   -> TyFloat
    W.Boolean           -> TyBoolean
    W.Byte              -> TyBuiltIn $ Name "Byte"
    W.Octet             -> TyBuiltIn $ Name "Octet"

data ArgType = NonOpt Argument
             | Opt Argument

partitionArgs :: [ArgType] -> ([Argument], [Argument])
partitionArgs [] = ([], [])
partitionArgs (Opt arg:rest) = let (args, optargs) = partitionArgs rest in (args, arg:optargs)
partitionArgs (NonOpt arg:rest) = let (args, optargs) = partitionArgs rest in (arg:args, optargs)

transArg :: W.Argument Tag -> Trans ArgType
transArg = \case
    W.ArgOptional _extAttrs ty (W.ArgIdent x) mDefault -> do
        ty' <- transType ty
        return $ Opt (Argument (i2n x) ty' mDefault)
    W.ArgNonOpt _extAttrs ty _mEllipsis (W.ArgIdent x) -> do
        ty' <- transType ty
        return $ NonOpt (Argument (i2n x) ty' Nothing)
    _ -> error "ArgKey is not supported yet"

inspectAttrComment :: [Comment] -> Trans ()
inspectAttrComment = mapM_ collectGhostAttr .
                     filter isGhostAttrComment .
                     map cmToSs
    where
        collectGhostAttr s = do
            let attrStr = strip $ drop 8 (strip s)
            case tryParse pAttribute attrStr of
                Right (W.Attribute _ _mInheritModifier _mReadOnlyModifer ty x) -> do
                    ITySingle ty' <- transType ty
                    emitGhostAttr ty' (i2n x)
                Left err -> throwError $ "Parse ghost attribute error: " ++ show err
        isGhostAttrComment  = startswith "/- ghost" . strip

analyzeConsAnn :: [Comment] -> (Maybe String, Maybe String, Maybe String)
analyzeConsAnn cms = let (e, r, eff, _) = analyzeFunAnn (map cmToSs cms) in (e, r, eff)

cmToSs :: Comment -> String
cmToSs (LineComment l) = l
cmToSs (BlockComment l) = l

analyzeFunAnn :: [String] -> (Maybe String, Maybe String, Maybe String, [CallbackSpec])
analyzeFunAnn blocks =
    let ens = fmap (strip . drop 10 . strip) $ listToMaybe $ filter isEnsureComment blocks
        req = fmap (strip . drop 11 . strip) $ listToMaybe $ filter isRequireComment blocks
        cbs = map (analyzeCBS . strip . drop 12 . strip) $ filter isCallbackComment blocks
        effs = fmap stripEffects $ listToMaybe $ filter isEffectComment blocks
    in  (ens, req, effs, cbs)
    where
        isEnsureComment  = startswith "/- ensures" . strip
        isRequireComment = startswith "/- requires" . strip
        isCallbackComment = startswith "/- callback" . strip
        isEffectComment = startswith "/- effects" . strip
        stripEffects s = let braced = strip (drop 10 (strip s)) in strip $ reverse (drop 1 (reverse (drop 1 braced)))

analyzeCBS :: String -> CallbackSpec
analyzeCBS s =
    let p = CallbackSpec <$> (i2n <$> pIdent) <*>
                             (spaces *> string "when" *> spaces *> string "(" *> pStringEnds ")") <*>
                             (spaces *> string "with" *> spaces *> (split ", " <$> pString))
    in case tryParse p s of
        Right cbs -> cbs
        Left err -> error $ "Parse CallbackSpec error: " ++ show err ++ " of " ++ s

analyzeOpAnn :: [Comment] -> Trans (Maybe String, Maybe String, Maybe String, [CallbackSpec])
analyzeOpAnn blocks = do
    inspectAttrComment blocks
    return $ analyzeFunAnn (map cmToSs blocks)

emitConst :: W.ConstType -> W.Ident -> W.ConstValue -> Trans ()
emitConst _ i cv = do
    modify (\s -> let DefInterface iface = (_focus s)
                      focus' = iface { _consts = M.insert (i2n i) (constValueToPrim cv) (_consts iface) }
                  in  s { _focus = DefInterface focus' })

cTyToIType :: W.ConstType -> Trans IType
cTyToIType (W.ConstPrim primTy _) = return $ transPrimType primTy
cTyToIType (W.ConstIdent i mNull) = do
    let name = i2n i
    tymap <- _typemap <$> get
    let ty = case M.lookup name tymap of
                Just (ITySingle ty) -> ty
                Just _ -> error "tyindent union"
                Nothing -> TyInterface name
    case mNull of
        Just W.Null -> return $ TyNullable ty
        Nothing     -> return ty

emitAttr :: IType -> Name -> Trans ()
emitAttr ty x =
    modify (\s -> let DefInterface iface = (_focus s)
                      focus' = iface { _attrs = M.insert x ty (_attrs iface) }
                  in  s { _focus = DefInterface focus' })

emitGhostAttr :: IType -> Name -> Trans ()
emitGhostAttr ty x =
    modify (\s -> let DefInterface iface = (_focus s)
                      focus' = iface { _ghostAttrs = M.insert x ty (_ghostAttrs iface) }
                  in  s { _focus = DefInterface focus' })

emitConstructor :: InterfaceConstructor -> Trans ()
emitConstructor cons =
    modify (\s -> let DefInterface iface = (_focus s)
                      InterfaceConstructors conss = _constructors iface
                      focus' = iface { _constructors = InterfaceConstructors (cons : conss) }
                  in  s { _focus = DefInterface focus' })

emitHTMLConstructor :: Trans ()
emitHTMLConstructor =
    modify (\s -> let DefInterface iface = (_focus s) in
                    case _constructors iface of
                        InterfaceConstructors [] -> 
                            s { _focus = DefInterface (iface { _constructors = InterfaceHTMLConstructor }) }
                        _ -> error "Illegal constructor")

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
nameOf (DefDictionary (Dictionary name _ _)) = name
nameOf (DefException (Exception name _ _)) = name
-- nameOf (DefEnum (Enum name _)) = name
nameOf (DefCallback (Callback name _ _)) = name

constValueToPrim = \case
    W.ConstBooleanLiteral b -> PBool b
    W.ConstFloatLiteral d -> PNumber d
    W.ConstInteger i -> PInt (fromIntegral i :: Int)
    W.ConstNull -> PNull
