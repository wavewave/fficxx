{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TH where

import Foreign.Ptr
import FFICXX.Runtime.Cast
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
--
import FFICXX.Runtime.CodeGen.Cxx
import FFICXX.Runtime.TH          ( FunctionParamInfo(..), IsCPrimitive(..), TemplateParamInfo(..) )
import FFICXX.Runtime.Function.Template
import FFICXX.Runtime.Function.TH ( genFunctionInstanceFor )

genFunctionInstanceFor
  [t|IO ()|]
  FPInfo {
    fpinfoCxxArgTypes = []
  , fpinfoCxxRetType = Nothing
  , fpinfoCxxHeaders =  []
  , fpinfoCxxNamespaces = []
  , fpinfoSuffix = "f1"
  }

genImplProxy :: Q [Dec]
genImplProxy = do
  addModFinalizer
    (addForeignSource LangCxx
      (concatMap
         (renderCMacro . Include)
         [ "MacroPatternMatch.h"
         , "functional"
         , "Function.h"
         , "ProxyTestImpl.h"
         , "test.h"
         ]
      <> (renderBlock $ ExternC $
           (   [ Verbatim
                 "class ImplProxy : public Impl {\n\
                 \private:\n\
                 \  std::function<void()>* fn;\n\
                 \public:\n\
                 \  ImplProxy( void* fp ) {\n\
                 \    fn = static_cast<std::function<void()>*>(fp);\n\
                 \  }\n\
                 \  virtual ~ImplProxy() {}\n\
                 \  virtual void action();\n\
                 \};\n"
               , Verbatim
                 "void ImplProxy::action() {\n\
                 \ (*(this->fn))();\n\
                 \}\n"
               ]
            <> (map CRegular
                  [ TypeDef (CTVerbatim "struct ImplProxy_tag") (sname "ImplProxy_t")
                  , TypeDef (CTVerbatim "ImplProxy_t *") (sname "ImplProxy_p")
                  , TypeDef (CTVerbatim "ImplProxy_t const*") (sname "const_ImplProxy_p")
                  , CMacroApp (sname "DELETABLE_DEF_VIRT") [sname "ImplProxy"]
                  , CMacroApp (sname "IMPL_DEF_VIRT")      [sname "ImplProxy"]
                  , CDefinition
                      Nothing
                      (CFunDecl
                        (CTVerbatim "ImplProxy_p")
                        (sname "ImplProxy_newImplProxy")
                        [(CTVerbatim "void*", sname "m")]
                      )
                      [ CInit (CVarDecl (CTVerbatim "ImplProxy*") (sname "newp"))
                              (CEVerbatim $ "new ImplProxy(m)")
                      , CReturn $ CEVerbatim "to_nonconst<ImplProxy_t,ImplProxy>(newp);"
                      ]
                  ]
               )
           )
         )
      )
    )
  pure []
