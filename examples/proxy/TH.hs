{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TH where

import Foreign.Ptr
import FFICXX.Runtime.Cast
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
--
import FFICXX.Runtime.CodeGen.C
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
      (   concatMap (renderCMacro . Include) [ "MacroPatternMatch.h"
                                             , "functional"
                                             , "Function.h"
                                             , "ProxyTestImpl.h"
                                             , "test.h"
                                             ]
       ++ "extern \"C\" {\n\
          \class ImplProxy : public Impl {\n\
          \private:\n\
          \  std::function<void()>* fn;\n\
          \public:\n\
          \  ImplProxy( void* fp ) {\n\
          \    fn = static_cast<std::function<void()>*>(fp);\n\
          \  }\n\
          \  virtual ~ImplProxy() {}\n\
          \  virtual void action();\n\
          \};\n\
          \\n\
          \void ImplProxy::action() {\n\
          \ (*(this->fn))();\n\
          \}\n\
          \\n\
          \typedef struct ImplProxy_tag ImplProxy_t;\n\
          \typedef ImplProxy_t * ImplProxy_p;\n\
          \typedef ImplProxy_t const* const_ImplProxy_p;\n\
          \\n\
          \DELETABLE_DEF_VIRT(ImplProxy)\n\
          \IMPL_DEF_VIRT(ImplProxy)\n\
          \\n\
          \\n\
          \ImplProxy_p ImplProxy_newImplProxy ( void* m ) {\n\
          \  ImplProxy* newp = new ImplProxy(m);\n\
          \  return to_nonconst<ImplProxy_t,ImplProxy>(newp);\n\
          \}\n\
          \\n\
          \} // extern C\n"
      )
    )
  pure []
