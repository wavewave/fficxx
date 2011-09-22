instance GADTTypeable $mother$ where
  data GADTType $mother$ a where 
$GADTbody$
    GADT$mother$Bottom  :: GADTType $mother$ BottomType
  data EGADTType $mother$ = forall a. EGADT$mother$ (GADTType $mother$ a)

cast$mother$ :: Exist $mother$ -> IO (EGADTType $mother$)
cast$mother$ eobj = do 
  let obj = $mother$ (get_fptr eobj)
  etclass <- isA obj  
  cname <- case etclass of 
             ETClass tclass -> getName tclass
  case cname of 
$castbody$
    _         -> return . EGADT$mother$ \$ GADT$mother$Bottom





