module XMLTypes where 

data SimpleXML =                                             
         PCDATA  String                                                     
       | Element ElementName [SimpleXML]
     deriving Show
                                                                              
type ElementName = String                                                    

xml2string :: SimpleXML -> String
xml2string (PCDATA s) =
  s
xml2string (Element tag []) =
 "<" ++ tag ++ "/>" 
xml2string (Element tag body) =
 "<" ++ tag ++ ">" ++ (concat (map xml2string body)) ++ "</" ++ tag ++ ">"


